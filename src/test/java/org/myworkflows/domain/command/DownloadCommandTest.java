package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowDownload;
import org.myworkflows.domain.WorkflowDownloadCompression;
import org.myworkflows.domain.WorkflowDownloadRequest;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.download.ContentDownloadSource;
import org.myworkflows.domain.download.LocalDownloadSource;
import org.myworkflows.domain.download.RemoteDownloadSource;
import org.myworkflows.domain.download.SshTarget;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.nio.file.Path;
import java.time.Instant;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class DownloadCommandTest {

    private final AtomicReference<WorkflowDownloadRequest> written = new AtomicReference<>();

    @Test
    public void whenPathIsGivenThenAFileOfThisServerIsOfferedAndItsIdIsReturned() {
        // given
        final var workflowRun = submittedRun();

        // when
        final var id = new DownloadCommand().download(workflowRun, " /var/log/app ", null, "logs", "auto", 10, null, null, null, 22, 60_000);

        // then
        assertEquals(workflowRun.getId() + "/logs.zip", id);
        final var request = written.get();
        assertEquals(new LocalDownloadSource(Path.of("/var/log/app")), request.source());
        assertEquals("logs", request.fileName());
        assertEquals(WorkflowDownloadCompression.AUTO, request.compression());
        assertEquals(10L * 1024 * 1024, request.compressAbove());
    }

    @Test
    public void whenContentIsGivenThenItNeedsAFileName() {
        // given
        final var workflowRun = submittedRun();

        // when
        new DownloadCommand().download(workflowRun, "", "a;b", "orders.csv", "never", 0.5, null, null, null, 22, 60_000);

        // then
        assertEquals(new ContentDownloadSource("a;b", "orders.csv"), written.get().source());
        assertEquals(512L * 1024, written.get().compressAbove());
        assertThrows(WorkflowRuntimeException.class,
            () -> new DownloadCommand().download(workflowRun, null, "a;b", " ", "auto", 10, null, null, null, 22, 60_000));
    }

    @Test
    public void whenPathAndContentAreBothGivenThenNothingIsOffered() {
        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> new DownloadCommand().download(submittedRun(), "/tmp/a", "text", "a.txt", "auto", 10, null, null, null, 22, 60_000));
        assertTrue(exception.getMessage().contains("not both"));
        assertNull(written.get());
    }

    @Test
    public void whenNeitherPathNorContentIsGivenThenNothingIsOffered() {
        // when and then
        assertThrows(WorkflowRuntimeException.class,
            () -> new DownloadCommand().download(submittedRun(), null, null, null, "auto", 10, null, null, null, 22, 60_000));
        assertNull(written.get());
    }

    @Test
    public void whenHostIsGivenThenThePathIsReadOverSsh() {
        // when
        new DownloadCommand().download(submittedRun(), "/var/log/app", "", null, "always", 10, " app-02 ", "deploy", "secret", 2222, 5_000);

        // then
        final var source = (RemoteDownloadSource) written.get().source();
        assertEquals(new SshTarget("app-02", 2222, "deploy", "secret", 5_000), source.target());
        assertEquals("/var/log/app", source.path());
        assertFalse(source.target().toString().contains("secret"));
    }

    @Test
    public void whenHostIsGivenWithoutCredentialsOrPathThenNothingIsOffered() {
        // when and then
        assertThrows(WorkflowRuntimeException.class,
            () -> new DownloadCommand().download(submittedRun(), "/var/log", null, null, "auto", 10, "app-02", null, null, 22, 5_000));
        assertThrows(WorkflowRuntimeException.class,
            () -> new DownloadCommand().download(submittedRun(), null, null, null, "auto", 10, "app-02", "deploy", "secret", 22, 5_000));
        assertNull(written.get());
    }

    @Test
    public void whenCompressionIsNotKnownThenNothingIsOffered() {
        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> new DownloadCommand().download(submittedRun(), "/tmp/a", null, null, "gzip", 10, null, null, null, 22, 60_000));
        assertTrue(exception.getMessage().contains("auto, always, never"));
    }

    @Test
    public void whenRunWasNotSubmittedThenItCannotOfferAFile() {
        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> new DownloadCommand().download(new WorkflowRun(), "/tmp/a", null, null, "auto", 10, null, null, null, 22, 60_000));
        assertTrue(exception.getMessage().contains("cannot offer files to download"));
    }

    private WorkflowRun submittedRun() {
        final var workflowRun = new WorkflowRun();
        workflowRun.setDownloadWriter((run, request) -> {
            written.set(request);
            final var fileName = request.source() instanceof ContentDownloadSource ? request.fileName() : request.fileName() + ".zip";
            return new WorkflowDownload(run.getId(), fileName, 1, Instant.now(), null, null);
        });
        return workflowRun;
    }

}
