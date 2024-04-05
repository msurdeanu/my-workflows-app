package org.myworkflows.domain;

import org.myworkflows.domain.command.AbstractCommand;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowBuilder {

    private final List<AbstractCommand> commands = new ArrayList<>();
    private final List<AbstractCommand> finallyCommands = new ArrayList<>();

    public void addCommand(final AbstractCommand abstractCommand) {
        commands.add(abstractCommand);
    }

    public void addFinallyCommand(final AbstractCommand abstractCommand) {
        finallyCommands.add(abstractCommand);
    }

    public WorkflowDefinition build(final String name) {
        final var workflow = new WorkflowDefinition();
        workflow.setName(name);
        workflow.setCommands(commands);
        workflow.setFinallyCommands(finallyCommands);
        return workflow;
    }

}
