package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.time.Duration;
import java.util.Set;

/**
 * Keeps the workflow run where it is until a user approves it in the UI. The command fails when the run is rejected or
 * when nobody decides in time, which stops the run like any other failure.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@NoArgsConstructor
public final class ApprovalCommand extends AbstractCommand {

    public static final String PREFIX = "approval";

    public ApprovalCommand(String name,
                           Set<ExpressionNameValue> ifs,
                           Set<ExpressionNameValue> inputs,
                           Set<ExpressionNameValue> asserts,
                           Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = PREFIX)
    public String approval(@ExecutionParam(bypassed = true) WorkflowRun workflowRun,
                           @ExecutionParam(required = false) String message,
                           @ExecutionParam(required = false, defaultValue = "3600000") Number timeout) throws InterruptedException {
        if (timeout.longValue() < 1) {
            throw new WorkflowRuntimeException("The timeout of an approval has to be a positive number of milliseconds");
        }
        return workflowRun.awaitApproval(getName(), message, Duration.ofMillis(timeout.longValue()));
    }

}
