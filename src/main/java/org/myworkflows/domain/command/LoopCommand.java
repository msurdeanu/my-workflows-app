package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;

import java.util.List;
import java.util.Set;

import static java.util.stream.IntStream.range;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class LoopCommand extends AbstractSubCommand {

    public static final String PREFIX = "loop";

    private static final String LOOP_ITEM = "loop.item";

    public LoopCommand(String name,
                       Set<ExpressionNameValue> ifs,
                       Set<ExpressionNameValue> inputs,
                       Set<ExpressionNameValue> asserts,
                       Set<ExpressionNameValue> outputs,
                       List<AbstractCommand> subcommands) {
        super(name, ifs, inputs, asserts, outputs, subcommands);
    }

    @ExecutionMethod(prefix = PREFIX)
    public int loop(@ExecutionParam(bypassed = true) WorkflowRun workflowRun,
                    @ExecutionParam List<Object> items,
                    @ExecutionParam(required = false, defaultValue = "1000") Number backoffPeriod) {
        final var previousLoopItem = workflowRun.getCache().find(LOOP_ITEM);
        try {
            range(0, items.size()).forEach(index -> {
                final var currentItem = items.get(index);
                workflowRun.getCache().put(LOOP_ITEM, currentItem);
                getSubcommands().forEach(subCommand -> subCommand.run(workflowRun, currentItem));
                if (index < items.size() - 1) {
                    sleepAWhile(backoffPeriod.longValue());
                }
            });
        } finally {
            previousLoopItem.ifPresentOrElse(
                item -> workflowRun.getCache().put(LOOP_ITEM, item),
                () -> workflowRun.getCache().remove(LOOP_ITEM)
            );
        }
        return items.size();
    }

}
