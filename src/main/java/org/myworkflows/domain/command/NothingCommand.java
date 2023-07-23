package org.myworkflows.domain.command;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class NothingCommand extends AbstractCommand {

    @ExecutionMethod
    public void nothing() {
        // This command is not doing anything. It can be used to inject input parameters.
    }

}