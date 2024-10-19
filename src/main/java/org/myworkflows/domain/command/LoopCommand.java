package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.util.Optional.ofNullable;
import static java.util.stream.IntStream.range;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class LoopCommand extends AbstractSubCommand {

    public static final String PREFIX = "loop";

    public LoopCommand(String name,
                       Set<ExpressionNameValue> ifs,
                       Set<ExpressionNameValue> inputs,
                       Set<ExpressionNameValue> asserts,
                       Set<ExpressionNameValue> outputs,
                       List<AbstractCommand> subcommands) {
        super(name, ifs, inputs, asserts, outputs, subcommands);
    }

    @ExecutionMethod(prefix = PREFIX)
    public Map<Object, Boolean> loop(@ExecutionParam WorkflowRun workflowRun,
                                     @ExecutionParam List<Object> items,
                                     @ExecutionParam(required = false, defaultValue = "1000") Number backoffPeriod) {
        final var result = new HashMap<Object, Boolean>();
        final var previousLoopItem = workflowRun.getCache().get("loop.item");
        try {
            range(0, items.size()).forEach(index -> {
                workflowRun.getCache().put("loop.item", items.get(index));
                getSubcommands().forEach(subCommand -> subCommand.run(workflowRun));
                if (index < items.size() - 1) {
                    sleepAWhile(backoffPeriod.longValue());
                }
            });
        } finally {
            ofNullable(previousLoopItem).ifPresent(item -> workflowRun.getCache().put("loop.item", item));
        }
        return result; // TODO
    }

}
