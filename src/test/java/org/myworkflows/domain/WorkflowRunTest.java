package org.myworkflows.domain;

import org.junit.jupiter.api.Test;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunTest {

    @Test
    public void testWorkflowRun() {
        final var workflowRun = new WorkflowRun(1, Map.of("key", "value"));
        assertEquals(1, workflowRun.getWorkflowTemplateId());
        assertNull(workflowRun.getUserId());
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

    @Test
    public void whenRunIsTriggeredByUserThenOnlyThatUserCanReplayIt() {
        // given
        final var workflowRun = new WorkflowRun(null, Map.of(), 7);
        assertEquals(7, workflowRun.getUserId());

        // when the run is not failed yet
        assertFalse(workflowRun.isReplayableBy(7));

        // then, once failed, it can be replayed by its user only
        workflowRun.markAsFailed(new WorkflowRuntimeException("test"), WorkflowDefinitionScript.empty());
        assertTrue(workflowRun.isEligibleForReplay());
        assertTrue(workflowRun.isReplayableBy(7));
        assertFalse(workflowRun.isReplayableBy(8));
        assertFalse(workflowRun.isReplayableBy(null));
    }

    @Test
    public void whenRunHasNoUserThenNobodyCanReplayIt() {
        // given a run scheduled by its template
        final var workflowRun = new WorkflowRun(1, Map.of());
        workflowRun.markAsFailed(new WorkflowRuntimeException("test"), WorkflowDefinitionScript.empty());

        // when and then
        assertTrue(workflowRun.isEligibleForReplay());
        assertFalse(workflowRun.isReplayableBy(7));
        assertFalse(workflowRun.isReplayableBy(null));
    }

}
