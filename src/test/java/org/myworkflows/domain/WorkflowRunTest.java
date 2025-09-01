package org.myworkflows.domain;

import org.junit.jupiter.api.Test;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunTest {

    @Test
    public void testWorkflowRun() {
        final var workflowRun = new WorkflowRun(1, Map.of("key", "value"));
        assertEquals(1, workflowRun.getWorkflowTemplateId());
        assertEquals("value", workflowRun.getCache().get("key"));
        assertEquals(0, workflowRun.getAllPrints().size());
        workflowRun.markKeyAsPrinted("key1");
        assertEquals(0, workflowRun.getAllPrints().size());
        workflowRun.markKeyAsPrinted("key");
        assertEquals(1, workflowRun.getAllPrints().size());
        workflowRun.markAsCompleted(1_000_000);
        assertEquals("16 mins and 40 secs", workflowRun.getHumanReadableDuration());
        assertFalse(workflowRun.isEligibleForReplay());
        workflowRun.markAsFailed(new WorkflowRuntimeException("test"), null);
        assertFalse(workflowRun.isEligibleForReplay());
    }

}
