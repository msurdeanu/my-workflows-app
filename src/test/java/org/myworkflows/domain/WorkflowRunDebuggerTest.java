package org.myworkflows.domain;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunVariable.Change;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowRunDebuggerTest {

    @Test
    public void whenRunIsOnlyWatchedThenItPausesNowhere() {
        // given
        final var debugger = WorkflowRunDebugger.watching();

        // when and then
        assertFalse(debugger.pausesBefore(0));
        assertFalse(debugger.pausesBefore(1));
    }

    @Test
    public void whenNoBreakpointIsGivenThenRunPausesBeforeItsFirstCommand() {
        // given
        final var debugger = WorkflowRunDebugger.pausingBefore(Set.of());

        // when and then
        assertTrue(debugger.pausesBefore(0));
    }

    @Test
    public void whenBreakpointsAreGivenThenRunPausesBeforeThoseCommandsOnly() {
        // given
        final var debugger = WorkflowRunDebugger.pausingBefore(Set.of(1, 3));

        // when and then
        assertFalse(debugger.pausesBefore(0));
        assertTrue(debugger.pausesBefore(1));
        assertFalse(debugger.pausesBefore(2));
        assertTrue(debugger.pausesBefore(3));
    }

    @Test
    public void whenRunIsToldToGoOnThenTheCommandItGoesOnWithDoesNotHoldItBack() {
        // given a run which goes on with a command a breakpoint stands on
        final var debugger = WorkflowRunDebugger.pausingBefore(Set.of(1));
        debugger.goOn(1, false, Set.of(1, 2));

        // when and then that command runs, and the next breakpoint holds the run
        assertFalse(debugger.pausesBefore(1));
        assertTrue(debugger.pausesBefore(2));
        // and the command holds the run back again, would it come back to it
        assertTrue(debugger.pausesBefore(1));
    }

    @Test
    public void whenRunStepsThenItPausesBeforeTheNextCommandWhateverItIs() {
        // given
        final var debugger = WorkflowRunDebugger.pausingBefore(Set.of(4));
        debugger.goOn(1, true, Set.of(4));

        // when and then
        assertFalse(debugger.pausesBefore(1));
        assertTrue(debugger.pausesBefore(2));

        // when it is told to carry on, then only the breakpoint holds it
        debugger.goOn(2, false, Set.of(4));
        assertFalse(debugger.pausesBefore(2));
        assertFalse(debugger.pausesBefore(3));
        assertTrue(debugger.pausesBefore(4));
    }

    @Test
    public void whenRunStandsStillThenWhatItHoldsIsComparedToTheLastTime() {
        // given
        final var cache = new WorkflowRunCache(UUID.randomUUID());
        cache.put("status", "open");
        final var debugger = WorkflowRunDebugger.watching();
        assertEquals(List.of(), debugger.getVariables());

        // when the run starts, then nothing is marked
        debugger.snapshot(cache);
        assertEquals(List.of(new WorkflowRunVariable("status", "string", "open", Change.NONE)), debugger.getVariables());

        // when a command adds a value and changes another
        cache.put("status", "closed");
        cache.put("count", 3);
        debugger.snapshot(cache);

        // then both are marked
        assertEquals(List.of(new WorkflowRunVariable("count", "integer", "3", Change.ADDED),
            new WorkflowRunVariable("status", "string", "closed", Change.CHANGED)), debugger.getVariables());

        // when nothing happens until the next time, then nothing is marked anymore
        debugger.snapshot(cache);
        assertTrue(debugger.getVariables().stream().noneMatch(WorkflowRunVariable::isMarked));
    }

}
