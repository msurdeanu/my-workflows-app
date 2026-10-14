package org.myworkflows.domain.command;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.List;

/**
 * Stands for a command whose type is not known, like one brought by a library which is not loaded anymore. A workflow
 * definition saved with such a command can still be read: the command keeps everything it was written with, so that it
 * is written back as it was, and it fails only when it is run.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@NoArgsConstructor
public final class UnknownCommand extends AbstractSubCommand {

    public static final String PREFIX = "unknown";

    /**
     * The value of the {@code class} field the command was written with.
     */
    @Getter
    @JsonProperty(value = "class", access = JsonProperty.Access.WRITE_ONLY)
    private String type;

    @Override
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    public List<AbstractCommand> getSubcommands() {
        return super.getSubcommands();
    }

    @ExecutionMethod(prefix = PREFIX)
    public void unknown() {
        throw new WorkflowRuntimeException("Command class '" + type + "' is not known: the library which brings it is probably not loaded");
    }

}
