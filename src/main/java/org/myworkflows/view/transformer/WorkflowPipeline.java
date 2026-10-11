package org.myworkflows.view.transformer;

import org.myworkflows.domain.WorkflowRun;

import java.util.List;
import java.util.stream.IntStream;

/**
 * The commands of a workflow in the order they run, described for somebody who does not read its definition. The final
 * commands are the ones which run whatever happens to the others.
 *
 * @author Mihai Surdeanu
 * @since 2.0
 */
public record WorkflowPipeline(List<Step> commands, List<Step> finallyCommands) {

    /**
     * @return how far the given run got on this pipeline, one status per command.
     */
    public Progress progressOf(WorkflowRun workflowRun) {
        // a run gets its duration when it ends, whereas its future is still running while the last progress is sent
        final var running = workflowRun.getDuration() < 0;
        final var done = Math.min(workflowRun.getLastSuccessfulIndex() + 1, commands.size());
        // a failure is the one of a final command when every other command has passed
        final var failed = workflowRun.getFailureMessage() != null && done < commands.size();
        final var commandStatuses = statusesOf(commands.size(), done, running && !failed, failed);

        // the final commands start once the others are over, and nothing but a failure stops them
        final var started = failed || done == commands.size();
        final var finallyDone = Math.min(workflowRun.getLastSuccessfulFinallyIndex() + 1, finallyCommands.size());
        final var finallyStatuses = statusesOf(finallyCommands.size(), finallyDone, running, !running);
        return new Progress(commandStatuses, running && !started ? pending(finallyCommands.size()) : finallyStatuses);
    }

    /**
     * @return the progress of a run which did not start yet.
     */
    public Progress noProgress() {
        return new Progress(pending(commands.size()), pending(finallyCommands.size()));
    }

    private static List<Status> statusesOf(int size, int done, boolean running, boolean failed) {
        return IntStream.range(0, size).mapToObj(index -> {
            if (index < done) {
                return Status.DONE;
            } else if (index == done && failed) {
                return Status.FAILED;
            } else if (running) {
                return index == done ? Status.RUNNING : Status.PENDING;
            }
            return Status.SKIPPED;
        }).toList();
    }

    private static List<Status> pending(int size) {
        return IntStream.range(0, size).mapToObj(_ -> Status.PENDING).toList();
    }

    /**
     * A command of the pipeline. Its type is the value of its {@code class} field and its detail is the one input worth
     * showing, empty unless the definition spells it out. A command which runs other commands lists them as sub-steps.
     *
     * @author Mihai Surdeanu
     * @since 2.0
     */
    public record Step(String name, String type, String detail, List<String> stores, List<String> conditions, List<Step> subSteps) {

    }

    /**
     * @author Mihai Surdeanu
     * @since 2.0
     */
    public record Progress(List<Status> commands, List<Status> finallyCommands) {

    }

    /**
     * @author Mihai Surdeanu
     * @since 2.0
     */
    public enum Status {
        PENDING, RUNNING, DONE, FAILED, SKIPPED
    }

}
