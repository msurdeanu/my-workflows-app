package org.myworkflows.domain.command;

import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;

/**
 * A command as a library would bring it: it is not one of the application, so a test has to register it first.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class EchoCommand extends AbstractCommand {

    public static final String PREFIX = "echo";

    @ExecutionMethod(prefix = PREFIX)
    public String echo(@ExecutionParam(bypassed = true) WorkflowRun workflowRun,
                       @ExecutionParam String text,
                       @ExecutionParam(required = false, defaultValue = "1") Number times) {
        return text.repeat(times.intValue());
    }

}
