package org.myworkflows.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.config.DownloadConfig;
import org.myworkflows.domain.WorkflowDownload;
import org.myworkflows.domain.WorkflowDownloadCompression;
import org.myworkflows.domain.WorkflowDownloadRequest;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.download.ContentDownloadSource;
import org.myworkflows.domain.download.DownloadSource;
import org.myworkflows.domain.download.LocalDownloadSource;
import org.myworkflows.domain.event.WorkflowDownloadOnWriteEvent;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.provider.SettingProvider;

import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.zip.ZipInputStream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowDownloadServiceTest {

    private static final long MB = 1024L * 1024L;

    @TempDir
    private Path tempDir;

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private SettingProvider settingProvider;

    @Mock
    private EventBroadcaster eventBroadcaster;

    private final DownloadConfig downloadConfig = new DownloadConfig();

    private WorkflowDownloadService workflowDownloadService;

    @BeforeEach
    public void init() {
        downloadConfig.setBaseDirectory(tempDir.resolve("downloads").toString());
        lenient().when(applicationManager.getBeanOfType(DownloadConfig.class)).thenReturn(downloadConfig);
        lenient().when(applicationManager.getBeanOfType(SettingProvider.class)).thenReturn(settingProvider);
        lenient().when(settingProvider.getOrDefault("downloadMaxSize", 500)).thenReturn(1);
        lenient().when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        workflowDownloadService = new WorkflowDownloadService(applicationManager);
    }

    @Test
    public void whenSmallTextIsOfferedThenItIsKeptAsItIsInTheDirectoryOfItsRun() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();

        // when
        final var workflowDownload = write(workflowRun, new ContentDownloadSource("id;total\n1;12", "orders.csv"), null,
            WorkflowDownloadCompression.AUTO, MB);

        // then
        assertEquals(workflowRun.getId() + "/orders.csv", workflowDownload.id());
        assertEquals("id;total\n1;12", Files.readString(runDirectory(workflowRun).resolve("orders.csv"), UTF_8));
        assertEquals(13, workflowDownload.size());
        assertFalse(workflowDownload.isZip());
        assertEquals(List.of(workflowDownload), workflowDownloadService.findAllOfRun(workflowRun.getId()));
        assertEquals(Optional.of(workflowDownload), workflowDownloadService.find(workflowRun.getId(), "orders.csv"));
        assertEquals(Optional.empty(), workflowDownloadService.find(workflowRun.getId(), "../orders.csv"));
        try (var inputStream = workflowDownloadService.openStream(workflowDownload)) {
            assertEquals("id;total\n1;12", new String(inputStream.readAllBytes(), UTF_8));
        }
        final var event = ArgumentCaptor.forClass(WorkflowDownloadOnWriteEvent.class);
        verify(eventBroadcaster).broadcast(event.capture());
        assertEquals(workflowDownload, event.getValue().workflowDownload());
    }

    @Test
    public void whenTextIsLargerThanTheThresholdThenItIsZipped() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var content = "line of a log\n".repeat(1_000);

        // when
        final var workflowDownload = write(workflowRun, new ContentDownloadSource(content, "app.log"), "today", WorkflowDownloadCompression.AUTO, 100);

        // then
        assertEquals("today.zip", workflowDownload.fileName());
        assertEquals(1, workflowDownload.zipEntries());
        assertEquals(content.length(), workflowDownload.zipContentSize());
        assertTrue(workflowDownload.size() < content.length());
        assertEquals(List.of("app.log"), zipEntryNames(runDirectory(workflowRun).resolve("today.zip")));
    }

    @Test
    public void whenFileIsCompressedAlreadyThenAutoKeepsItAsItIs() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var file = Files.writeString(tempDir.resolve("backup.tar.gz"), "x".repeat(500));

        // when
        final var workflowDownload = write(workflowRun, new LocalDownloadSource(file), null, WorkflowDownloadCompression.AUTO, 10);

        // then
        assertEquals("backup.tar.gz", workflowDownload.fileName());
        assertFalse(workflowDownload.isZip());
    }

    @Test
    public void whenCompressionIsAlwaysThenEvenASmallFileIsZipped() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var file = Files.writeString(tempDir.resolve("notes.txt"), "a note");

        // when
        final var workflowDownload = write(workflowRun, new LocalDownloadSource(file), null, WorkflowDownloadCompression.ALWAYS, MB);

        // then
        assertEquals("notes.txt.zip", workflowDownload.fileName());
        assertEquals(List.of("notes.txt"), zipEntryNames(runDirectory(workflowRun).resolve("notes.txt.zip")));
    }

    @Test
    public void whenDirectoryIsOfferedThenItIsZippedWithItsFilesUnderItsName() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var directory = Files.createDirectories(tempDir.resolve("logs"));
        Files.writeString(directory.resolve("a.log"), "first");
        Files.writeString(Files.createDirectories(directory.resolve("old")).resolve("b.log"), "second");

        // when
        final var workflowDownload = write(workflowRun, new LocalDownloadSource(directory), null, WorkflowDownloadCompression.AUTO, MB);

        // then
        assertEquals("logs.zip", workflowDownload.fileName());
        assertEquals(2, workflowDownload.zipEntries());
        assertEquals(11, workflowDownload.zipContentSize());
        assertEquals(List.of("logs/a.log", "logs/old/b.log"), zipEntryNames(runDirectory(workflowRun).resolve("logs.zip")));
    }

    @Test
    public void whenDirectoryIsOfferedWithoutCompressionThenNothingIsWritten() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var directory = Files.createDirectories(tempDir.resolve("logs"));
        Files.writeString(directory.resolve("a.log"), "first");

        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> write(workflowRun, new LocalDownloadSource(directory), null, WorkflowDownloadCompression.NEVER, MB));
        assertTrue(exception.getMessage().contains("cannot be 'never'"));
        assertEquals(List.of(), workflowDownloadService.findAllOfRun(workflowRun.getId()));
    }

    @Test
    public void whenRunOffersTwoFilesWithTheSameNameThenTheSecondGetsANumberBeforeItsExtension() {
        // given
        final var workflowRun = new WorkflowRun();

        // when
        write(workflowRun, new ContentDownloadSource("first", "backup.tar.gz"), null, WorkflowDownloadCompression.NEVER, MB);
        final var second = write(workflowRun, new ContentDownloadSource("second", "backup.tar.gz"), null, WorkflowDownloadCompression.NEVER, MB);

        // then
        assertEquals("backup (2).tar.gz", second.fileName());
        assertEquals(2, workflowDownloadService.findAllOfRun(workflowRun.getId()).size());
    }

    @Test
    public void whenFileIsLargerThanTheSettingThenItIsRefusedBeforeItIsCopied() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var file = Files.write(tempDir.resolve("big.bin"), new byte[(int) (2 * MB)]);

        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> write(workflowRun, new LocalDownloadSource(file), null, WorkflowDownloadCompression.NEVER, MB));
        assertTrue(exception.getMessage().contains("larger than the 1 MB"));
        assertFalse(Files.exists(runDirectory(workflowRun)));
    }

    @Test
    public void whenZipGrowsLargerThanTheSettingThenItStopsAndLeavesNothingBehind() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var randomBytes = new byte[(int) (2 * MB)];
        new Random(42).nextBytes(randomBytes);
        final var file = Files.write(tempDir.resolve("random.bin"), randomBytes);

        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> write(workflowRun, new LocalDownloadSource(file), null, WorkflowDownloadCompression.ALWAYS, MB));
        assertTrue(exception.getMessage().contains("larger than the 1 MB"));
        try (var files = Files.list(runDirectory(workflowRun))) {
            assertEquals(0, files.count());
        }
        verify(eventBroadcaster, never()).broadcast(any());
    }

    @Test
    public void whenRunIsCancelledWhileItCopiesThenItStopsAndLeavesNothingBehind() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        Thread.currentThread().interrupt();

        // when and then
        try {
            final var exception = assertThrows(WorkflowRuntimeException.class,
                () -> write(workflowRun, new ContentDownloadSource("text", "a.txt"), null, WorkflowDownloadCompression.NEVER, MB));
            assertTrue(exception.getMessage().contains("cancelled"));
        } finally {
            assertTrue(Thread.interrupted());
        }
        assertEquals(List.of(), workflowDownloadService.findAllOfRun(workflowRun.getId()));
    }

    @Test
    public void whenPathIsOutsideTheAllowedDirectoriesThenItIsRefused() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var allowed = Files.createDirectories(tempDir.resolve("allowed"));
        final var inside = Files.writeString(allowed.resolve("in.txt"), "in");
        final var outside = Files.writeString(tempDir.resolve("out.txt"), "out");
        downloadConfig.setAllowedDirectories(new ArrayList<>(List.of(allowed.toString())));

        // when and then
        assertEquals("in.txt", write(workflowRun, new LocalDownloadSource(inside), null, WorkflowDownloadCompression.NEVER, MB).fileName());
        final var escaping = allowed.resolve("..").resolve("out.txt");
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> write(workflowRun, new LocalDownloadSource(escaping), null, WorkflowDownloadCompression.NEVER, MB));
        assertTrue(exception.getMessage().contains("not in a directory a download may read from"));
        assertTrue(Files.exists(outside));
    }

    @Test
    public void whenPathDoesNotExistThenNothingIsOffered() {
        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> write(new WorkflowRun(),
            new LocalDownloadSource(tempDir.resolve("missing.txt")), null, WorkflowDownloadCompression.AUTO, MB));
        assertTrue(exception.getMessage().contains("missing.txt"));
    }

    @Test
    public void whenRequestedNameHoldsAPathThenOnlyANameIsKept() {
        // when
        final var workflowDownload = write(new WorkflowRun(), new ContentDownloadSource("text", "a.txt"), "../../etc/passwd",
            WorkflowDownloadCompression.NEVER, MB);

        // then
        assertEquals("-..-etc-passwd", workflowDownload.fileName());
    }

    @Test
    public void whenRunsAreDeletedThenTheirFilesAreDeletedWithThem() {
        // given
        final var deletedRun = new WorkflowRun();
        final var keptRun = new WorkflowRun();
        write(deletedRun, new ContentDownloadSource("text", "a.txt"), null, WorkflowDownloadCompression.NEVER, MB);
        write(keptRun, new ContentDownloadSource("text", "b.txt"), null, WorkflowDownloadCompression.NEVER, MB);

        // when
        workflowDownloadService.deleteAllOf(List.of(deletedRun.getId()));

        // then
        assertFalse(Files.exists(runDirectory(deletedRun)));
        assertEquals(1, workflowDownloadService.findAllOfRun(keptRun.getId()).size());
    }

    @Test
    public void whenFileIsBeingWrittenThenItIsNotListed() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        Files.writeString(Files.createDirectories(runDirectory(workflowRun)).resolve(".1234.part"), "half");

        // when and then
        assertEquals(List.of(), workflowDownloadService.findAllOfRun(workflowRun.getId()));
    }

    @Test
    public void whenNameHasAnExtensionThenItIsSplitBeforeIt() {
        // when and then
        assertArrayEquals(new String[] {"orders", ".csv"}, WorkflowDownloadService.splitExtension("orders.csv"));
        assertArrayEquals(new String[] {"backup", ".tar.gz"}, WorkflowDownloadService.splitExtension("backup.tar.gz"));
        assertArrayEquals(new String[] {"README", ""}, WorkflowDownloadService.splitExtension("README"));
        assertArrayEquals(new String[] {".tar", ".gz"}, WorkflowDownloadService.splitExtension(".tar.gz"));
        assertTrue(WorkflowDownloadService.isCompressedAlready("photo.JPG"));
        assertFalse(WorkflowDownloadService.isCompressedAlready("app.log"));
    }

    @Test
    public void whenZipCannotBeReadThenItIsListedAsAFile() throws Exception {
        // given
        final var workflowRun = new WorkflowRun();
        final var workflowDownload = write(workflowRun, new ContentDownloadSource("not a zip", "fake.zip"), null, WorkflowDownloadCompression.NEVER, MB);

        // when and then
        assertEquals("fake.zip", workflowDownload.fileName());
        assertNull(workflowDownload.zipEntries());
    }

    private WorkflowDownload write(WorkflowRun workflowRun, DownloadSource source, String fileName, WorkflowDownloadCompression compression,
                                   long compressAbove) {
        return workflowDownloadService.write(workflowRun, new WorkflowDownloadRequest(source, fileName, compression, compressAbove));
    }

    private Path runDirectory(WorkflowRun workflowRun) {
        return workflowDownloadService.baseDirectory().resolve(workflowRun.getId().toString());
    }

    private static List<String> zipEntryNames(Path zipPath) throws Exception {
        final var names = new ArrayList<String>();
        try (var zipInputStream = new ZipInputStream(new ByteArrayInputStream(Files.readAllBytes(zipPath)))) {
            for (var entry = zipInputStream.getNextEntry(); entry != null; entry = zipInputStream.getNextEntry()) {
                names.add(entry.getName());
            }
        }
        return names;
    }

}
