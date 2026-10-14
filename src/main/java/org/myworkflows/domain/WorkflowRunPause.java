package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.time.Duration;
import java.time.Instant;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * What a workflow run waits for while it is paused before one of its commands: somebody who tells it how to go on, or
 * to stop. Like an approval, it is settled for good by whatever comes first - an answer, the deadline or the run being
 * cancelled: an answer which comes after that is refused.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@RequiredArgsConstructor
public final class WorkflowRunPause {

    /**
     * The position, among the commands of the run, of the command the run is paused before.
     */
    @Getter
    private final int commandIndex;

    @Getter
    private final String commandName;

    @Getter
    private final Instant deadline;

    private final CompletableFuture<Resume> answer = new CompletableFuture<>();

    /**
     * @return whether the run was waiting for this answer, and follows it.
     */
    public boolean resume(Resume resume) {
        return answer.complete(resume);
    }

    /**
     * Waits to be told how to go on.
     *
     * @return what the run was told, which is to stop when nobody answered before the deadline.
     * @throws InterruptedException when the run is cancelled while it waits.
     */
    Resume await() throws InterruptedException {
        answer.completeOnTimeout(Resume.TIMED_OUT, Duration.between(Instant.now(), deadline).toMillis(), TimeUnit.MILLISECONDS);
        try {
            return answer.get();
        } catch (ExecutionException exception) {
            throw new WorkflowRuntimeException(exception);
        } finally {
            // whoever answers after the run stopped waiting is told so
            answer.cancel(false);
        }
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public enum Action {
        /**
         * Run one command, then pause again.
         */
        STEP,
        /**
         * Run until a command the run has to pause before, or until the end.
         */
        CONTINUE,
        /**
         * Run no command anymore. The final commands still run.
         */
        STOP
    }

    /**
     * How a paused run goes on.
     *
     * @param commandIndex the command to go on with, which is another one than the run is paused before when it is told
     *     to run from somewhere else.
     * @param script the script to go on with, when it was changed while the run was paused. The run owns it from now on,
     *     hence it has to be a copy nobody else holds.
     * @param breakpoints the commands to pause before from now on.
     * @param username who tells the run to go on or to stop.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public record Resume(Action action, int commandIndex, WorkflowDefinitionScript script, Set<Integer> breakpoints, String username) {

        private static final Resume TIMED_OUT = new Resume(Action.STOP, 0, null, Set.of(), null);

        public static Resume stop(String username) {
            return new Resume(Action.STOP, 0, null, Set.of(), username);
        }

        public boolean isTimedOut() {
            return this == TIMED_OUT;
        }

    }

}
