package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * What a workflow run waits for when it reaches an approval command: a user who approves it or rejects it, before a
 * deadline. Whichever comes first - an approval, a rejection, the deadline or the run being cancelled - settles it for
 * good: a decision which comes after that is refused.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@RequiredArgsConstructor
public final class WorkflowRunApproval {

    /**
     * The name of the command which asks for the approval.
     */
    @Getter
    private final String commandName;

    /**
     * What the approver is told, or nothing when the command does not say.
     */
    @Getter
    private final String message;

    @Getter
    private final Instant deadline;

    private final CompletableFuture<Decision> decision = new CompletableFuture<>();

    /**
     * @return whether the run was waiting for this decision, and goes on because of it.
     */
    public boolean approve(String username) {
        return decision.complete(new Decision(true, username));
    }

    /**
     * @return whether the run was waiting for this decision, and fails because of it.
     */
    public boolean reject(String username) {
        return decision.complete(new Decision(false, username));
    }

    /**
     * Waits for the decision.
     *
     * @return the name of the user who approved.
     * @throws WorkflowRuntimeException when the approval was rejected, or when nobody decided before the deadline.
     * @throws InterruptedException when the run is cancelled while it waits.
     */
    String await() throws InterruptedException {
        decision.completeOnTimeout(Decision.NONE, Duration.between(Instant.now(), deadline).toMillis(), TimeUnit.MILLISECONDS);
        try {
            final var made = decision.get();
            if (made.approved()) {
                return made.username();
            }
            throw new WorkflowRuntimeException(made == Decision.NONE
                ? "Nobody approved or rejected the run before the approval timed out"
                : "The run was rejected by '" + made.username() + "'");
        } catch (ExecutionException exception) {
            throw new WorkflowRuntimeException(exception);
        } finally {
            // whoever decides after the run stopped waiting is told so
            decision.cancel(false);
        }
    }

    private record Decision(boolean approved, String username) {

        private static final Decision NONE = new Decision(false, null);

    }

}
