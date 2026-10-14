package org.myworkflows.domain;

import java.util.List;
import java.util.Set;

import static java.util.Optional.ofNullable;

/**
 * Follows a workflow run for whoever develops its definition: it says before which commands the run has to pause, and
 * keeps what the run held the last time it stood still - when it started, each time it paused and when it ended. A run
 * has one only when it is started from the development page, and it is not saved, like everything about a run in
 * progress.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowRunDebugger {

    private static final int NO_COMMAND = -1;

    private volatile Set<Integer> breakpoints;

    /**
     * Whether the run pauses before its next command, whichever it is.
     */
    private volatile boolean stepping;

    /**
     * The command the run was told to go on with, which holds it back no more.
     */
    private volatile int resumedAt = NO_COMMAND;

    private volatile List<WorkflowRunVariable> variables;

    private WorkflowRunDebugger(Set<Integer> breakpoints, boolean stepping) {
        this.breakpoints = Set.copyOf(breakpoints);
        this.stepping = stepping;
    }

    /**
     * @return a debugger for a run which pauses nowhere: what it held is looked at once it is over.
     */
    public static WorkflowRunDebugger watching() {
        return new WorkflowRunDebugger(Set.of(), false);
    }

    /**
     * @return a debugger for a run which pauses before the given commands, or before the first command it runs when no
     *     command is given.
     */
    public static WorkflowRunDebugger pausingBefore(Set<Integer> breakpoints) {
        return new WorkflowRunDebugger(breakpoints, breakpoints.isEmpty());
    }

    /**
     * Changes the commands to pause before while the run is on its way.
     */
    public void setBreakpoints(Set<Integer> newBreakpoints) {
        breakpoints = Set.copyOf(newBreakpoints);
    }

    /**
     * Tells how the run goes on from the given command, which it runs whatever the breakpoints say.
     *
     * @param step whether the run pauses again after that one command.
     */
    public void goOn(int commandIndex, boolean step, Set<Integer> newBreakpoints) {
        breakpoints = Set.copyOf(newBreakpoints);
        stepping = step;
        resumedAt = commandIndex;
    }

    /**
     * To be asked once before every command, by the run itself: the command the run was told to go on with is
     * forgotten once it was asked about.
     *
     * @return whether the run has to pause before the given command.
     */
    public boolean pausesBefore(int commandIndex) {
        final var resumed = commandIndex == resumedAt;
        resumedAt = NO_COMMAND;
        return !resumed && (stepping || breakpoints.contains(commandIndex));
    }

    /**
     * Writes down what the run holds, and marks what was added or changed since it was written down the last time: the
     * first time, there is nothing to compare with and nothing is marked. It is called on the thread of the run, as
     * nothing else can read what a run holds while its commands change it.
     */
    public void snapshot(WorkflowRunCache cache) {
        variables = WorkflowRunVariable.allOf(cache, variables);
    }

    /**
     * @return what the run held the last time it stood still.
     */
    public List<WorkflowRunVariable> getVariables() {
        return ofNullable(variables).orElse(List.of());
    }

}
