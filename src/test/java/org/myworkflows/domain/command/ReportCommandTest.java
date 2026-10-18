package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class ReportCommandTest {

    @Test
    public void whenReportIsWrittenThenItsIdIsReturned() {
        // given
        final var workflowRun = new WorkflowRun();
        final var written = new AtomicReference<List<Object>>();
        workflowRun.setReportWriter((run, title, format, content) -> {
            written.set(List.of(run, title, format, content));
            return new WorkflowReport(run.getId(), title + format.getExtension(), format, null, null, content.length(), Instant.now());
        });
        final var reportCommand = new ReportCommand("Write it", Set.of(), Set.of(), Set.of(), Set.of());

        // when and then
        assertEquals(workflowRun.getId() + "/Weekly.html", reportCommand.report(workflowRun, "Weekly", "<p>Sales</p>", "html"));
        assertEquals(List.of(workflowRun, "Weekly", WorkflowReportFormat.HTML, "<p>Sales</p>"), written.get());
    }

    @Test
    public void whenFormatIsNotKnownThenNothingIsWritten() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.setReportWriter((run, title, format, content) -> {
            throw new AssertionError("nothing is written");
        });

        // when and then
        assertThrows(WorkflowRuntimeException.class, () -> new ReportCommand().report(workflowRun, "Weekly", "text", "pdf"));
    }

    @Test
    public void whenRunWasNotSubmittedThenItCannotWriteAReport() {
        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> new ReportCommand().report(new WorkflowRun(), "Weekly", "text", "markdown"));
        assertTrue(exception.getMessage().contains("cannot write reports"));
    }

}
