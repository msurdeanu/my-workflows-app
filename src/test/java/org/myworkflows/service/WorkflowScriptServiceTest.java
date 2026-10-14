package org.myworkflows.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunApproval;
import org.myworkflows.domain.WorkflowRunDebugger;
import org.myworkflows.domain.WorkflowRunPause;
import org.myworkflows.domain.WorkflowRunPause.Action;
import org.myworkflows.domain.WorkflowRunPause.Resume;
import org.myworkflows.domain.WorkflowRunVariable;
import org.myworkflows.domain.WorkflowRunVariable.Change;
import org.myworkflows.domain.event.Event;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.serializer.SerializerFactory;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import static java.util.Optional.ofNullable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowScriptServiceTest {

    private static final String SCRIPT = """
        commands:
        - class: nothing
          name: Nothing command
          inputs:
          - name: greeting
            value: $$(MY_VALUE)
        """;

    private static final String SCRIPT_WITH_APPROVAL = """
        commands:
        - class: approval
          name: Approve
        - class: nothing
          name: Go on
        finallyCommands:
        - class: nothing
          name: Clean up
        """;

    private static final String SCRIPT_WITH_UNKNOWN_COMMAND = """
        commands:
        - class: nothing
          name: Start
        - class: gone
          name: Brought by a library
        """;

    private static final String SCRIPT_WITH_THREE_COMMANDS = """
        commands:
        - class: nothing
          name: First
          inputs:
          - name: first
            value: done
        - class: nothing
          name: Second
          inputs:
          - name: second
            value: done
        - class: nothing
          name: Third
          inputs:
          - name: third
            value: done
        finallyCommands:
        - class: nothing
          name: Clean up
          inputs:
          - name: cleaned
            value: done
        """;

    private static final int WAIT_IN_SECONDS = 30;

    private static final int PAUSE_IN_MILLIS = 1_000;

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private EventBroadcaster eventBroadcaster;

    @Mock
    private WorkflowPlaceholderService workflowPlaceholderService;

    @Test
    public void whenTheSameScriptIsRunTwiceThenPlaceholdersAreResolvedAgainstTheCurrentValues() throws Exception {
        // given a single script instance, as it is shared by the internal cache across runs
        final var sharedScript = toObject(SCRIPT, WorkflowDefinitionScript.class);
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        when(applicationManager.getBeanOfType(WorkflowPlaceholderService.class)).thenReturn(workflowPlaceholderService);

        // when the placeholder value changes between the two runs
        when(workflowPlaceholderService.getAllAsMap()).thenReturn(Map.of("MY_VALUE", "first"));
        final var firstRun = run(workflowScriptService, sharedScript, executorService);
        when(workflowPlaceholderService.getAllAsMap()).thenReturn(Map.of("MY_VALUE", "second"));
        final var secondRun = run(workflowScriptService, sharedScript, executorService);
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(30, TimeUnit.SECONDS));

        // then each run sees the value that was current when it started
        assertEquals("first", firstRun.getCache().get("greeting"));
        assertEquals("second", secondRun.getCache().get("greeting"));
        // and the shared script is left untouched, so it can still be edited and persisted as authored
        assertTrue(SerializerFactory.toString(sharedScript, "").contains("$$(MY_VALUE)"));
    }

    @Test
    public void whenRunWaitsForAnApprovalThenItIsFollowedAndGoesOnOnceApproved() throws Exception {
        // given
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pendingApproval = followApprovals();
        final var workflowRun = new WorkflowRun();

        // when
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_APPROVAL);
        final var approval = pendingApproval.get(30, TimeUnit.SECONDS);

        // then the run stands at its first command
        assertTrue(workflowRun.isWaitingForApproval());
        assertEquals(-1, workflowRun.getLastSuccessfulIndex());

        // when
        assertTrue(approval.approve("john"));
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(30, TimeUnit.SECONDS));

        // then
        assertNull(workflowRun.getFailureMessage());
        assertEquals(1, workflowRun.getLastSuccessfulIndex());
        assertFalse(workflowRun.isWaitingForApproval());
    }

    @Test
    public void whenRunIsRejectedThenItFailsAtTheApprovalAndItsFinalCommandsStillRun() throws Exception {
        // given
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pendingApproval = followApprovals();
        final var workflowRun = new WorkflowRun();

        // when
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_APPROVAL);
        assertTrue(pendingApproval.get(30, TimeUnit.SECONDS).reject("john"));
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(30, TimeUnit.SECONDS));

        // then
        assertTrue(workflowRun.getFailureMessage().contains("Command 'Approve' failed"));
        assertTrue(workflowRun.getFailureMessage().contains("The run was rejected by 'john'."));
        assertEquals(-1, workflowRun.getLastSuccessfulIndex());
        assertEquals(0, workflowRun.getLastSuccessfulFinallyIndex());
    }

    @Test
    public void whenScriptHasACommandOfAnUnknownTypeThenTheRunFailsAtThatCommand() throws Exception {
        // given a script read while the library which brings one of its commands is not loaded
        final var script = SerializerFactory.toObjectKeepingUnknownCommands(SCRIPT_WITH_UNKNOWN_COMMAND, WorkflowDefinitionScript.class);
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);

        // when
        final var workflowRun = run(workflowScriptService, script, executorService);
        executorService.shutdown();

        // then
        assertEquals(0, workflowRun.getLastSuccessfulIndex());
        assertTrue(workflowRun.getFailureMessage().contains("Command class 'gone' is not known: the library which brings it is probably not loaded."));
    }

    @Test
    public void whenRunIsDebuggedThenItPausesAtItsBreakpointStepsAndCarriesOn() throws Exception {
        // given a run which has to pause before its second command
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pauses = followPauses();
        final var workflowRun = new WorkflowRun();
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of(1)));

        // when
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_THREE_COMMANDS);
        final var firstPause = pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS);

        // then it stands before that command, with what the first command left: nothing is marked, as nobody looked before
        assertEquals(1, firstPause.getCommandIndex());
        assertEquals("Second", firstPause.getCommandName());
        assertTrue(workflowRun.isPaused());
        assertEquals(0, workflowRun.getLastSuccessfulIndex());
        assertEquals(List.of(new WorkflowRunVariable("first", "string", "done", Change.NONE)), workflowRun.getDebugger().getVariables());

        // when it steps
        Thread.sleep(PAUSE_IN_MILLIS);
        assertTrue(firstPause.resume(new Resume(Action.STEP, 1, null, Set.of(1), "john")));
        final var secondPause = pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS);

        // then it ran one command, and only what that command left is marked
        assertEquals(2, secondPause.getCommandIndex());
        assertEquals("Third", secondPause.getCommandName());
        assertEquals(List.of(new WorkflowRunVariable("first", "string", "done", Change.NONE),
            new WorkflowRunVariable("second", "string", "done", Change.ADDED)), workflowRun.getDebugger().getVariables());

        // when it carries on
        assertTrue(secondPause.resume(new Resume(Action.CONTINUE, 2, null, Set.of(1), "john")));
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

        // then it runs to its end, final commands included
        assertNull(workflowRun.getFailureMessage());
        assertFalse(workflowRun.isPaused());
        assertEquals(2, workflowRun.getLastSuccessfulIndex());
        assertEquals(0, workflowRun.getLastSuccessfulFinallyIndex());
        assertEquals(List.of("cleaned", "third"), workflowRun.getDebugger().getVariables().stream()
            .filter(WorkflowRunVariable::isMarked)
            .map(WorkflowRunVariable::name)
            .toList());
        // and the time it spent paused is no part of how long it took
        assertTrue(workflowRun.getDuration() < PAUSE_IN_MILLIS, "The run took " + workflowRun.getDuration() + " ms.");
    }

    @Test
    public void whenRunIsOnlyWatchedThenItNeverPausesAndWhatItHeldIsKept() throws Exception {
        // given a run started from the development page with Run
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pauses = followPauses();
        final var workflowRun = new WorkflowRun(Map.of("status", "open"));
        workflowRun.setDebugger(WorkflowRunDebugger.watching());

        // when
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_THREE_COMMANDS);
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

        // then
        assertTrue(pauses.isEmpty());
        assertEquals(2, workflowRun.getLastSuccessfulIndex());
        assertEquals(List.of("cleaned", "first", "second", "status", "third"), workflowRun.getDebugger().getVariables().stream()
            .map(WorkflowRunVariable::name)
            .toList());
        // and nothing is marked, as the run never stood still before
        assertTrue(workflowRun.getDebugger().getVariables().stream().noneMatch(WorkflowRunVariable::isMarked));
    }

    @Test
    public void whenPausedRunIsStoppedThenItFailsThereAndItsFinalCommandsStillRun() throws Exception {
        // given
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pauses = followPauses();
        final var workflowRun = new WorkflowRun();
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of(1)));

        // when
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_THREE_COMMANDS);
        assertTrue(pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS).resume(Resume.stop("john")));
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

        // then
        assertEquals("The run was stopped by 'john' while it was paused before command 'Second'.", workflowRun.getFailureMessage());
        assertEquals(0, workflowRun.getLastSuccessfulIndex());
        assertEquals(0, workflowRun.getLastSuccessfulFinallyIndex());
        assertNull(workflowRun.getCache().get("second"));
        // and it can be replayed from where it stood
        assertTrue(workflowRun.isEligibleForReplay());
    }

    @Test
    public void whenNobodyTellsAPausedRunHowToGoOnThenItStopsOnceThePauseTimesOut() throws Exception {
        // given a pause which does not last
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager, Duration.ofMillis(50));
        final var pauses = followPauses();
        final var workflowRun = new WorkflowRun();
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of()));

        // when
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_THREE_COMMANDS);
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

        // then it stood before its first command, as no breakpoint was given
        assertEquals(0, pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS).getCommandIndex());
        assertEquals("The run was paused before command 'First' and nobody told it how to go on before the pause timed out.",
            workflowRun.getFailureMessage());
        assertEquals(-1, workflowRun.getLastSuccessfulIndex());
        assertEquals(0, workflowRun.getLastSuccessfulFinallyIndex());
    }

    @Test
    public void whenPausedRunIsCancelledThenItFailsThereAndItsFinalCommandsStillRun() throws Exception {
        // given
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pauses = followPauses();
        final var workflowRun = new WorkflowRun();
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of(2)));

        // when
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_THREE_COMMANDS);
        final var pause = pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS);
        assertTrue(workflowRun.cancelAndInterruptIfRunning());
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

        // then
        assertEquals("The run was cancelled while it was paused before command 'Third'.", workflowRun.getFailureMessage());
        assertEquals(1, workflowRun.getLastSuccessfulIndex());
        assertEquals(0, workflowRun.getLastSuccessfulFinallyIndex());
        assertFalse(pause.resume(new Resume(Action.CONTINUE, 2, null, Set.of(), "john")));
    }

    @Test
    public void whenPausedRunIsHandedAnotherScriptAndCommandThenItGoesOnWithThem() throws Exception {
        // given a run paused before its second command
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pauses = followPauses();
        final var workflowRun = new WorkflowRun();
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of(1)));
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_THREE_COMMANDS);
        final var pause = pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS);

        // when it is told to run from its third command, with the script as it was changed in the meantime
        final var changedScript = toObject(SCRIPT_WITH_THREE_COMMANDS.replace("value: done", "value: changed"), WorkflowDefinitionScript.class);
        assertTrue(pause.resume(new Resume(Action.CONTINUE, 2, changedScript, Set.of(), "john")));
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

        // then the second command is skipped, and what runs is what the changed script says - its final commands too
        assertNull(workflowRun.getFailureMessage());
        assertEquals(2, workflowRun.getLastSuccessfulIndex());
        assertEquals("done", workflowRun.getCache().get("first"));
        assertNull(workflowRun.getCache().get("second"));
        assertEquals("changed", workflowRun.getCache().get("third"));
        assertEquals("changed", workflowRun.getCache().get("cleaned"));
    }

    @Test
    public void whenPausedRunIsToldToRunFromAnEarlierCommandThenThatCommandRunsAgain() throws Exception {
        // given a run paused before its third command
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        final var pauses = followPauses();
        final var workflowRun = new WorkflowRun();
        workflowRun.setDebugger(WorkflowRunDebugger.pausingBefore(Set.of(2)));
        submit(workflowScriptService, workflowRun, SCRIPT_WITH_THREE_COMMANDS);
        final var pause = pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS);
        workflowRun.getCache().remove("first");

        // when it steps from its first command
        assertTrue(pause.resume(new Resume(Action.STEP, 0, null, Set.of(2), "john")));
        final var nextPause = pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS);

        // then that command ran again and the run stands before its second one
        assertEquals(1, nextPause.getCommandIndex());
        assertEquals("done", workflowRun.getCache().get("first"));

        // when it carries on, then the breakpoint holds it again
        assertTrue(nextPause.resume(new Resume(Action.CONTINUE, 1, null, Set.of(2), "john")));
        final var lastPause = pauses.poll(WAIT_IN_SECONDS, TimeUnit.SECONDS);
        assertEquals(2, lastPause.getCommandIndex());
        assertTrue(lastPause.resume(Resume.stop("john")));
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));
    }

    /**
     * @return the pauses of the runs, in the order the runs tell that they stand at one.
     */
    private BlockingQueue<WorkflowRunPause> followPauses() {
        final var pauses = new LinkedBlockingQueue<WorkflowRunPause>();
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        when(applicationManager.getBeanOfType(WorkflowPlaceholderService.class)).thenReturn(workflowPlaceholderService);
        doAnswer(invocation -> {
            if (invocation.getArgument(0) instanceof WorkflowDefinitionOnProgressEvent progressEvent) {
                ofNullable(progressEvent.workflowRun().getPause()).ifPresent(pauses::add);
            }
            return null;
        }).when(eventBroadcaster).broadcast(any(Event.class));
        return pauses;
    }

    /**
     * @return the approval of the first run which tells that it waits for one.
     */
    private CompletableFuture<WorkflowRunApproval> followApprovals() {
        final var pendingApproval = new CompletableFuture<WorkflowRunApproval>();
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        doAnswer(invocation -> {
            if (invocation.getArgument(0) instanceof WorkflowDefinitionOnProgressEvent progressEvent) {
                ofNullable(progressEvent.workflowRun().getApproval()).ifPresent(pendingApproval::complete);
            }
            return null;
        }).when(eventBroadcaster).broadcast(any(Event.class));
        return pendingApproval;
    }

    private void submit(WorkflowScriptService workflowScriptService, WorkflowRun workflowRun, String script) {
        workflowScriptService.onEventReceived(WorkflowDefinitionOnSubmitEvent.builder()
            .workflowRun(workflowRun)
            .workflowDefinitionScript(toObject(script, WorkflowDefinitionScript.class))
            .build());
    }

    private WorkflowRun run(WorkflowScriptService workflowScriptService, WorkflowDefinitionScript script,
                            ExecutorService executorService) throws Exception {
        final var workflowRun = new WorkflowRun();
        workflowScriptService.onEventReceived(WorkflowDefinitionOnSubmitEvent.builder()
            .workflowRun(workflowRun)
            .workflowDefinitionScript(script)
            .build());
        // the run is submitted to the executor, so wait for it to drain before looking at the cache
        executorService.submit(() -> { }).get(30, TimeUnit.SECONDS);
        return workflowRun;
    }

}
