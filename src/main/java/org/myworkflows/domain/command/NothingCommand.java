package org.myworkflows.domain.command;

import org.myworkflows.domain.command.api.ExecutionMethod;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class NothingCommand extends AbstractCommand {

    @ExecutionMethod(prefix = "nothing")
    public void nothing() {
        // This command is not doing anything. It can be used to inject input parameters.
    }

}