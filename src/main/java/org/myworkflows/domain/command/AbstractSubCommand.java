package org.myworkflows.domain.command;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public abstract class AbstractSubCommand extends AbstractCommand {

    @JsonProperty("subcommands")
    private List<AbstractCommand> subcommands = List.of();

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
