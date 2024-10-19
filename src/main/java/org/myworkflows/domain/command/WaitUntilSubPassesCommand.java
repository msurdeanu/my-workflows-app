package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;

import java.util.List;
import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class WaitUntilSubPassesCommand extends AbstractSubCommand {

    public static final String PREFIX = "waitUntilSubPasses";

    public WaitUntilSubPassesCommand(String name,
                                     Set<ExpressionNameValue> ifs,
                                     Set<ExpressionNameValue> inputs,
                                     Set<ExpressionNameValue> asserts,
                                     Set<ExpressionNameValue> outputs,
                                     List<AbstractCommand> subcommands) {
        super(name, ifs, inputs, asserts, outputs, subcommands);
    }

    @ExecutionMethod(prefix = PREFIX)
    public int waitUntilSubPasses(@ExecutionParam WorkflowRun workflowRun,
                                  @ExecutionParam(required = false, defaultValue = "3") Number iterations,
                                  @ExecutionParam(required = false, defaultValue = "1000") Number backoffPeriod) {
        int remainingIterations = iterations.intValue();
        while (remainingIterations-- > 0) {
            try {
                getSubcommands().forEach(subcommand -> subcommand.run(workflowRun));
                break;
            } catch (Exception notUsed) {
                sleepAWhile(backoffPeriod.longValue());
            }
        }

        return remainingIterations;
    }

}
