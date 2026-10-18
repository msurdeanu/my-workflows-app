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
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.config.ReportConfig;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.WorkflowReportOnChangeEvent;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.provider.SettingProvider;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowReportServiceTest {

    @TempDir
    private Path tempDir;

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private InternalCacheManager internalCacheManager;

    @Mock
    private SettingProvider settingProvider;

    @Mock
    private EventBroadcaster eventBroadcaster;

    private final InternalCache reportCache = new InternalCache(CacheNameEnum.WORKFLOW_REPORT.getName(), Integer.MAX_VALUE,
        InternalCache.InternalCacheOrder.LIFO);

    private final InternalCache templateCache = new InternalCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName());

    private WorkflowReportService workflowReportService;

    @BeforeEach
    public void init() {
        final var reportConfig = new ReportConfig();
        reportConfig.setBaseDirectory(tempDir.resolve("reports").toString());
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(CacheNameEnum.WORKFLOW_REPORT.getName())).thenReturn(reportCache);
        when(internalCacheManager.getCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName())).thenReturn(templateCache);
        lenient().when(applicationManager.getBeanOfType(ReportConfig.class)).thenReturn(reportConfig);
        lenient().when(applicationManager.getBeanOfType(SettingProvider.class)).thenReturn(settingProvider);
        lenient().when(settingProvider.getOrDefault("reportMaxSize", 5)).thenReturn(1);
        lenient().when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        workflowReportService = new WorkflowReportService(applicationManager);
    }

    @Test
    public void whenReportIsWrittenThenItIsAFileNamedAfterItsTitleInTheDirectoryOfItsRun() throws Exception {
        // given
        final var workflowRun = new WorkflowRun(7, null, 3);

        // when
        final var workflowReport = workflowReportService.write(workflowRun, "  Weekly sales - week 41 ", WorkflowReportFormat.MARKDOWN, "# Sales");

        // then
        final var filePath = runDirectory(workflowRun).resolve("Weekly sales - week 41.md");
        assertEquals("# Sales", Files.readString(filePath, UTF_8));
        assertEquals("Weekly sales - week 41", workflowReport.title());
        assertEquals(workflowRun.getId() + "/Weekly sales - week 41.md", workflowReport.id());
        assertEquals(WorkflowReportFormat.MARKDOWN, workflowReport.format());
        assertEquals(7, workflowReport.workflowTemplateId());
        assertEquals(3, workflowReport.userId());
        assertEquals(7, workflowReport.size());
        assertEquals(Optional.of(workflowReport), workflowReportService.findById(workflowReport.id()));
        assertEquals(List.of(workflowReport), workflowReportService.findAllOfRun(workflowRun.getId()).toList());
        assertEquals(Optional.of("# Sales"), workflowReportService.findContent(workflowReport));
        final var event = ArgumentCaptor.forClass(WorkflowReportOnChangeEvent.class);
        verify(eventBroadcaster).broadcast(event.capture());
        assertEquals(List.of(workflowReport), event.getValue().workflowReports());
        assertFalse(event.getValue().deleted());
    }

    @Test
    public void whenTwoReportsOfARunHaveTheSameTitleThenTheSecondFileGetsANumber() {
        // given
        final var workflowRun = new WorkflowRun();

        // when
        final var first = workflowReportService.write(workflowRun, "Status", WorkflowReportFormat.HTML, "<p>1</p>");
        final var second = workflowReportService.write(workflowRun, "Status", WorkflowReportFormat.HTML, "<p>2</p>");

        // then both files are kept, and each report reads its own
        assertEquals("Status.html", first.fileName());
        assertEquals("Status (2).html", second.fileName());
        assertEquals("Status (2)", second.title());
        assertEquals(Optional.of("<p>1</p>"), workflowReportService.findContent(first));
        assertEquals(Optional.of("<p>2</p>"), workflowReportService.findContent(second));
        // the newest one comes first
        assertEquals(List.of(second, first), workflowReportService.getAll().toList());
    }

    @Test
    public void whenReportIsLargerThanTheSettingAllowsThenNothingIsWritten() {
        // given a content of one byte more than the one MB of the setting
        final var workflowRun = new WorkflowRun();
        final var content = "x".repeat(1024 * 1024 + 1);

        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> workflowReportService.write(workflowRun, "Big", WorkflowReportFormat.MARKDOWN, content));
        assertTrue(exception.getMessage().contains("more than the 1 MB a report may have"));
        assertFalse(Files.exists(runDirectory(workflowRun)));
    }

    @Test
    public void whenTitleOrContentIsMissingThenNothingIsWritten() {
        // given
        final var workflowRun = new WorkflowRun();

        // when and then
        assertThrows(WorkflowRuntimeException.class, () -> workflowReportService.write(workflowRun, " ", WorkflowReportFormat.MARKDOWN, "text"));
        assertThrows(WorkflowRuntimeException.class, () -> workflowReportService.write(workflowRun, null, WorkflowReportFormat.MARKDOWN, "text"));
        assertThrows(WorkflowRuntimeException.class, () -> workflowReportService.write(workflowRun, "x".repeat(151), WorkflowReportFormat.MARKDOWN, "text"));
        assertThrows(WorkflowRuntimeException.class, () -> workflowReportService.write(workflowRun, "Title", WorkflowReportFormat.MARKDOWN, null));
        assertFalse(Files.exists(runDirectory(workflowRun)));
    }

    @Test
    public void whenReportIsDeletedThenItsFileAndTheEmptyDirectoryOfItsRunGoAway() {
        // given
        final var workflowRun = new WorkflowRun();
        final var workflowReport = workflowReportService.write(workflowRun, "Status", WorkflowReportFormat.MARKDOWN, "ok");

        // when
        workflowReportService.delete(workflowReport);

        // then
        assertEquals(Optional.empty(), workflowReportService.findById(workflowReport.id()));
        assertFalse(Files.exists(runDirectory(workflowRun)));
    }

    @Test
    public void whenRunsAreDeletedThenTheirReportsGoAwayAndTheOnesOfOtherRunsStay() throws Exception {
        // given a run with a report and a file copied by hand next to it, and another run
        final var deletedRun = new WorkflowRun();
        final var keptRun = new WorkflowRun();
        workflowReportService.write(deletedRun, "Status", WorkflowReportFormat.MARKDOWN, "gone");
        Files.writeString(runDirectory(deletedRun).resolve("notes.txt"), "gone too");
        final var keptReport = workflowReportService.write(keptRun, "Status", WorkflowReportFormat.MARKDOWN, "kept");

        // when
        workflowReportService.deleteAllOf(List.of(deletedRun.getId()));

        // then
        assertEquals(List.of(keptReport), workflowReportService.getAll().toList());
        assertFalse(Files.exists(runDirectory(deletedRun)));
        assertEquals(Optional.of("kept"), workflowReportService.findContent(keptReport));
    }

    @Test
    public void whenFileOfAReportIsGoneThenItHasNoContent() throws Exception {
        // given
        final var workflowReport = workflowReportService.write(new WorkflowRun(), "Status", WorkflowReportFormat.MARKDOWN, "ok");
        Files.delete(workflowReportService.filePathOf(workflowReport));

        // when and then
        assertEquals(Optional.empty(), workflowReportService.findContent(workflowReport));
    }

    @Test
    public void whenFileNameOfAReportLeavesTheDirectoryOfItsRunThenItIsNotRead() {
        // given
        final var workflowReport = new WorkflowReport(UUID.randomUUID(), "../../secret.md", WorkflowReportFormat.MARKDOWN, null, null, 1, null);

        // when and then
        assertThrows(WorkflowRuntimeException.class, () -> workflowReportService.findContent(workflowReport));
    }

    @Test
    public void whenFileIsDescribedThenOnlyAReportInTheDirectoryOfARunIsOne() throws Exception {
        // given
        final var runId = UUID.randomUUID();
        final var runDirectory = Files.createDirectories(tempDir.resolve("reports").resolve(runId.toString()));
        final var otherDirectory = Files.createDirectories(tempDir.resolve("reports").resolve("not-a-run"));
        final var report = Files.writeString(runDirectory.resolve("Copied by hand.HTML"), "<p/>");
        final var notes = Files.writeString(runDirectory.resolve("notes.txt"), "text");
        final var elsewhere = Files.writeString(otherDirectory.resolve("Status.md"), "text");

        // when
        final var workflowReport = workflowReportService.describe(report, 4, null).orElseThrow();

        // then
        assertEquals(runId, workflowReport.workflowRunId());
        assertEquals(WorkflowReportFormat.HTML, workflowReport.format());
        assertEquals("Copied by hand", workflowReport.title());
        assertEquals(4, workflowReport.workflowTemplateId());
        assertEquals(4, workflowReport.size());
        assertEquals(Optional.empty(), workflowReportService.describe(notes, null, null));
        assertEquals(Optional.empty(), workflowReportService.describe(elsewhere, null, null));
    }

    @Test
    public void whenReportHasATemplateThenItIsFoundInTheCacheOfTheTemplates() {
        // given
        final var workflowTemplate = WorkflowTemplate.of("Weekly sales", List.of(), List.of());
        templateCache.put(5, workflowTemplate);
        final var withTemplate = workflowReportService.write(new WorkflowRun(5, null, null), "Status", WorkflowReportFormat.MARKDOWN, "ok");
        final var withoutTemplate = workflowReportService.write(new WorkflowRun(), "Status", WorkflowReportFormat.MARKDOWN, "ok");

        // when and then
        assertSame(workflowTemplate, workflowReportService.findWorkflowTemplate(withTemplate).orElseThrow());
        assertEquals(Optional.empty(), workflowReportService.findWorkflowTemplate(withoutTemplate));
    }

    @Test
    public void whenTitleIsTurnedIntoAFileNameThenWhatAFileNameCannotHoldIsReplaced() {
        assertEquals("Weekly sales - week 41", WorkflowReportService.toFileTitle("Weekly sales - week 41"));
        assertEquals("Café déjà vu!", WorkflowReportService.toFileTitle("Café déjà vu!"));
        assertEquals("Sales 2026-10-", WorkflowReportService.toFileTitle("Sales 2026/10?"));
        assertEquals("-..-etc-passwd", WorkflowReportService.toFileTitle("../../etc/passwd"));
        assertEquals("report", WorkflowReportService.toFileTitle(".."));
        assertEquals("Two lines", WorkflowReportService.toFileTitle("Two\n  lines. "));
        assertEquals("CON-", WorkflowReportService.toFileTitle("CON"));
        assertEquals("nul.txt-", WorkflowReportService.toFileTitle("nul.txt"));
    }

    private Path runDirectory(WorkflowRun workflowRun) {
        return tempDir.resolve("reports").resolve(workflowRun.getId().toString());
    }

}
