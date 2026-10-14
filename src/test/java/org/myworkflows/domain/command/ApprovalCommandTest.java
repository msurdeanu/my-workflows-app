package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunApproval;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.util.ExceptionUtil;

import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static java.util.Optional.ofNullable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class ApprovalCommandTest {

    private static final int WAIT_IN_SECONDS = 30;

    private final WorkflowRun workflowRun = new WorkflowRun();

    private final AtomicInteger changes = new AtomicInteger();

    private final CompletableFuture<WorkflowRunApproval> pendingApproval = new CompletableFuture<>();

    @Test
    public void whenRunIsApprovedThenCommandGivesTheNameOfWhoApproved() throws Exception {
        // given
        final var command = createCommand(new ExpressionNameValue("approval.message", "Go live?", RuntimeEvaluator.PLAIN));
        followChanges();

        try (var executor = Executors.newSingleThreadExecutor()) {
            // when the command is run
            final var commandRun = executor.submit(() -> command.run(workflowRun));
            final var approval = pendingApproval.get(WAIT_IN_SECONDS, TimeUnit.SECONDS);

            // then the run waits, and whoever follows it was told
            assertTrue(workflowRun.isWaitingForApproval());
            assertEquals("Approve", approval.getCommandName());
            assertEquals("Go live?", approval.getMessage());
            assertEquals(1, changes.get());

            // when it is approved
            assertTrue(approval.approve("john"));
            commandRun.get(WAIT_IN_SECONDS, TimeUnit.SECONDS);

            // then the command is over, and a second decision is refused
            assertEquals("john", workflowRun.getCache().get("approvedBy"));
            assertFalse(workflowRun.isWaitingForApproval());
            assertEquals(2, changes.get());
            assertFalse(approval.reject("jane"));
        }
    }

    @Test
    public void whenRunIsRejectedThenCommandFails() throws Exception {
        // given
        final var command = createCommand();
        followChanges();

        try (var executor = Executors.newSingleThreadExecutor()) {
            // when
            final var commandRun = executor.submit(() -> command.run(workflowRun));
            final var approval = pendingApproval.get(WAIT_IN_SECONDS, TimeUnit.SECONDS);
            assertEquals("", approval.getMessage());
            assertTrue(approval.reject("john"));

            // then
            final var exception = assertThrows(ExecutionException.class, () -> commandRun.get(WAIT_IN_SECONDS, TimeUnit.SECONDS));
            assertTrue(ExceptionUtil.getMessageAndCause(exception.getCause()).contains("The run was rejected by 'john'."));
            assertNull(workflowRun.getCache().get("approvedBy"));
            assertFalse(workflowRun.isWaitingForApproval());
        }
    }

    @Test
    public void whenNobodyDecidesInTimeThenCommandFails() {
        // given
        final var command = createCommand(new ExpressionNameValue("approval.timeout", 50, RuntimeEvaluator.PLAIN));
        followChanges();

        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> command.run(workflowRun));
        assertTrue(ExceptionUtil.getMessageAndCause(exception).contains("before the approval timed out"));
        assertFalse(workflowRun.isWaitingForApproval());
        // whoever decides too late is told so
        assertFalse(pendingApproval.join().approve("john"));
    }

    @Test
    public void whenRunIsCancelledWhileItWaitsThenCommandFails() throws Exception {
        // given
        final var command = createCommand();
        followChanges();

        try (var executor = Executors.newSingleThreadExecutor()) {
            // when
            final var commandRun = executor.submit(() -> command.run(workflowRun));
            final var approval = pendingApproval.get(WAIT_IN_SECONDS, TimeUnit.SECONDS);
            assertTrue(commandRun.cancel(true));
            executor.shutdown();
            assertTrue(executor.awaitTermination(WAIT_IN_SECONDS, TimeUnit.SECONDS));

            // then
            assertFalse(workflowRun.isWaitingForApproval());
            assertFalse(approval.approve("john"));
        }
    }

    @Test
    public void whenTimeoutIsNotPositiveThenCommandFailsWithoutWaiting() {
        // given
        final var command = createCommand(new ExpressionNameValue("approval.timeout", 0, RuntimeEvaluator.PLAIN));
        followChanges();

        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> command.run(workflowRun));
        assertTrue(ExceptionUtil.getMessageAndCause(exception).contains("positive number of milliseconds"));
        assertEquals(0, changes.get());
    }

    private ApprovalCommand createCommand(ExpressionNameValue... inputs) {
        return new ApprovalCommand("Approve", Set.of(), Set.of(inputs), Set.of(),
            Set.of(new ExpressionNameValue("approvedBy", "#output", RuntimeEvaluator.SPEL)));
    }

    /**
     * Stands for whoever follows the run: it is told when the run starts and stops to wait, which is also the way a
     * test gets hold of the approval the run waits for.
     */
    private void followChanges() {
        workflowRun.setChangeListener(() -> {
            changes.incrementAndGet();
            ofNullable(workflowRun.getApproval()).ifPresent(pendingApproval::complete);
        });
    }

}
