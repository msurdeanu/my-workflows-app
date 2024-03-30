package org.myworkflows.domain;

import org.myworkflows.domain.Workflow;
import org.myworkflows.domain.command.AbstractCommand;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowBuilder {

    private final List<AbstractCommand> commands = new ArrayList<>();
    private final List<AbstractCommand> finallyCommands = new ArrayList<>();
    private final Set<String> tags = new HashSet<>();

    public void addCommand(final AbstractCommand abstractCommand) {
        commands.add(abstractCommand);
    }

    public void addFinallyCommand(final AbstractCommand abstractCommand) {
        finallyCommands.add(abstractCommand);
    }

    public void addTag(final String tag) {
        tags.add(tag);
    }

    public Workflow build(final String name) {
        final var workflow = new Workflow();
        workflow.setName(name);
        workflow.setTags(tags);
        workflow.setCommands(commands);
        workflow.setFinallyCommands(finallyCommands);
        return workflow;
    }

}
