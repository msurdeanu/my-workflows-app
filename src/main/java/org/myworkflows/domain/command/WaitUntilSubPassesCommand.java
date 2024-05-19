package org.myworkflows.domain.command;

import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;
import org.myworkflows.domain.command.api.OptionalParam;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WaitUntilSubPassesCommand extends AbstractSubCommand {

    @ExecutionMethod
    public int waitUntilSubPasses(@MandatoryParam ExecutionContext executionContext,
                                  @OptionalParam Number iterations,
                                  @OptionalParam Number backoffPeriod) {
        final var resolvedIterations = ofNullable(iterations).orElse(3).intValue();
        final var resolvedBackoffPeriod = ofNullable(backoffPeriod).orElse(1_000).longValue();

        int remainingIterations = resolvedIterations;
        while (remainingIterations-- > 0) {
            try {
                getSubcommands().forEach(subcommand -> subcommand.run(executionContext));
                break;
            } catch (Exception notUsed) {
                sleepAWhile(resolvedBackoffPeriod);
            }
        }

        return remainingIterations;
    }

}
