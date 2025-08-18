package org.myworkflows.domain.command;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;

import java.util.List;
import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor
@Getter
public abstract class AbstractSubCommand extends AbstractCommand {

    @JsonProperty("subcommands")
    private List<AbstractCommand> subcommands = List.of();

    public AbstractSubCommand(String name,
                              Set<ExpressionNameValue> ifs,
                              Set<ExpressionNameValue> inputs,
                              Set<ExpressionNameValue> asserts,
                              Set<ExpressionNameValue> outputs,
                              List<AbstractCommand> subcommands) {
        super(name, ifs, inputs, asserts, outputs);
        this.subcommands = subcommands;
    }

    protected void sleepAWhile(long millis) {
        if (millis < 1) {
            return;
        }

        try {
            Thread.sleep(millis);
        } catch (InterruptedException notUsed) {
            Thread.currentThread().interrupt();
        }
    }

}
