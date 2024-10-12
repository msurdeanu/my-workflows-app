package org.myworkflows.domain.command;

import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WaitUntilSubPassesCommand extends AbstractSubCommand {

    @ExecutionMethod(prefix = "waitUntilSubPasses")
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
