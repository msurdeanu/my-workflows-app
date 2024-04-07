package org.myworkflows.domain;

import lombok.Getter;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.util.ExceptionUtil;

import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.lang.String.join;
import static org.myworkflows.util.LangUtil.pluralize;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ExecutionContext {

    @Getter
    private final ExecutionCache cache = new ExecutionCache();

    private final Set<String> printedKeys = new LinkedHashSet<>();

    @Getter
    private final UUID workflowId;

    private final int totalCommands;

    private final List<String> commandNames;

    private int completedCommands;

    @Getter
    private String failureMessage;

    private long duration;

    public ExecutionContext() {
        workflowId = UUID.randomUUID();
        totalCommands = 0;
        commandNames = List.of();
    }

    public ExecutionContext(final WorkflowDefinition workflowDefinition) {
        workflowId = workflowDefinition.getId();
        totalCommands = workflowDefinition.getCommands().size() + workflowDefinition.getFinallyCommands().size();
        commandNames = new ArrayList<>(totalCommands);
        for (AbstractCommand command : workflowDefinition.getCommands()) {
            commandNames.add(command.getName());
        }
        for (AbstractCommand command : workflowDefinition.getFinallyCommands()) {
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

    public String getHumanReadableDuration() {
        if (duration < 1_000) {
            return duration + " ms";
        }

        final var parts = new ArrayList<String>(3);
        var remainingDuration = Duration.ofMillis(duration);
        pluralize("hr", remainingDuration.toHours(), true).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusHours(remainingDuration.toHours());
        pluralize("min", remainingDuration.toMinutes(), true).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusMinutes(remainingDuration.toMinutes());
        pluralize("sec", remainingDuration.toSeconds(), true).ifPresent(parts::add);

        return join(" and ", parts);
    }

}
