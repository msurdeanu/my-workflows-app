package org.myworkflows.domain.command;

import org.myworkflows.domain.command.api.ExecutionMethod;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class NothingCommand extends AbstractCommand {

    public static final String PREFIX = "nothing";

    @ExecutionMethod(prefix = PREFIX)
    public void nothing() {
        // Nothing to do. Used only for injecting parameters!
    }

}