package org.myworkflows.domain;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunPause.Action;
import org.myworkflows.domain.WorkflowRunPause.Resume;
import org.myworkflows.domain.WorkflowRunVariable.Change;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static java.util.Optional.ofNullable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
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
    public void whenRunIsPausedThenWhoeverFollowsItIsToldAndFindsWhatItHolds() throws Exception {
        // given a debugged run which somebody follows
        final var workflowRun = new WorkflowRun(null, Map.of("status", "open"), 7);
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of()));
        final var changes = new AtomicInteger();
        final var pendingPause = new CompletableFuture<WorkflowRunPause>();
        workflowRun.setChangeListener(() -> {
            changes.incrementAndGet();
            ofNullable(workflowRun.getPause()).ifPresent(pendingPause::complete);
        });

        try (var executor = Executors.newSingleThreadExecutor()) {
            // when the run pauses
            final var told = executor.submit(() -> workflowRun.awaitResume(1, "Build the report", Duration.ofSeconds(30)));
            final var pause = pendingPause.get(30, TimeUnit.SECONDS);

            // then
            assertTrue(workflowRun.isPaused());
            assertEquals(1, pause.getCommandIndex());
            assertEquals("Build the report", pause.getCommandName());
            assertEquals(List.of(new WorkflowRunVariable("status", "string", "open", Change.NONE)), workflowRun.getDebugger().getVariables());
            assertEquals(1, changes.get());

            // when it is told to go on
            final var step = new Resume(Action.STEP, 1, null, Set.of(), "john");
            assertTrue(pause.resume(step));

            // then
            assertSame(step, told.get(30, TimeUnit.SECONDS));
            assertFalse(workflowRun.isPaused());
            assertEquals(2, changes.get());
        }
    }

    @Test
    public void whenRunIsMovedToACommandThenTheCommandsBeforeItAreTakenAsPassed() {
        // given
        final var workflowRun = new WorkflowRun();
        assertEquals(-1, workflowRun.getLastSuccessfulIndex());

        // when and then
        workflowRun.moveTo(3);
        assertEquals(2, workflowRun.getLastSuccessfulIndex());
        workflowRun.moveTo(0);
        assertEquals(-1, workflowRun.getLastSuccessfulIndex());
        workflowRun.moveTo(-5);
        assertEquals(-1, workflowRun.getLastSuccessfulIndex());
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
