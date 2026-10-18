package org.myworkflows.domain;

import org.junit.jupiter.api.Test;
import org.myworkflows.exception.WorkflowRuntimeException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class WorkflowReportFormatTest {

    @Test
    public void whenFormatIsNamedInAnyCaseThenItIsFound() {
        assertEquals(WorkflowReportFormat.MARKDOWN, WorkflowReportFormat.of("markdown"));
        assertEquals(WorkflowReportFormat.MARKDOWN, WorkflowReportFormat.of(" Markdown "));
        assertEquals(WorkflowReportFormat.HTML, WorkflowReportFormat.of("HTML"));
    }

    @Test
    public void whenFormatIsNotKnownThenItIsRefused() {
        assertThrows(WorkflowRuntimeException.class, () -> WorkflowReportFormat.of("pdf"));
        assertThrows(WorkflowRuntimeException.class, () -> WorkflowReportFormat.of(null));
    }

}
