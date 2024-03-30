package org.myworkflows.domain;

import lombok.Getter;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.util.ExceptionUtil;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ExecutionContext {

    @Getter
    private final ExecutionCache cache = new ExecutionCache();

    private final Set<String> printedKeys = new LinkedHashSet<>();

    private final int totalCommands;

    private final List<String> commandNames;

    private int completedCommands;

    @Getter
    private String failureMessage;

    private long duration;

    public ExecutionContext() {
        totalCommands = 0;
        commandNames = List.of();
    }

    public ExecutionContext(final Workflow workflow) {
        totalCommands = workflow.getCommands().size() + workflow.getFinallyCommands().size();
        commandNames = new ArrayList<>(totalCommands);
        for (AbstractCommand command : workflow.getCommands()) {
            commandNames.add(command.getName());
        }
        for (AbstractCommand command : workflow.getFinallyCommands()) {
            commandNames.add(command.getName());
        }
    }

    public boolean isRunCompleted() {
        return completedCommands == totalCommands;
    }

    public List<ExecutionPrint> getAllPrints() {
        return printedKeys.stream()
                .flatMap(item -> cache.find(item).map(value -> new ExecutionPrint(item, value)).stream())
                .collect(Collectors.toList());
    }

    public boolean markKeyAsPrinted(final String key) {
        return cache.find(key)
                .map(value -> printedKeys.add(key))
                .orElse(false);
    }

    public void markCommandAsFailed(final Throwable throwable) {
        completedCommands = totalCommands;
        this.failureMessage = ExceptionUtil.getMessageAndCause(throwable);
    }

    public void markCommandAsCompleted() {
        completedCommands++;
    }

    public void markAsCompleted(final long duration) {
        this.duration = duration;
    }

}
