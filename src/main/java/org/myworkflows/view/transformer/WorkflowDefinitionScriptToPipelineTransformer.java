package org.myworkflows.view.transformer;

import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.AbstractSubCommand;
import org.myworkflows.domain.command.ApprovalCommand;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.command.DatabaseCommand;
import org.myworkflows.domain.command.GroovyCommand;
import org.myworkflows.domain.command.HttpRequestCommand;
import org.myworkflows.domain.command.JavaCommand;
import org.myworkflows.domain.command.MailCommand;
import org.myworkflows.domain.command.PrintCommand;
import org.myworkflows.domain.command.ReportCommand;
import org.myworkflows.domain.command.SleepCommand;
import org.myworkflows.domain.command.SshExecCommand;
import org.myworkflows.domain.command.SshShellCommand;
import org.myworkflows.domain.command.WaitUntilSubPassesCommand;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static java.util.Map.entry;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.joining;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class WorkflowDefinitionScriptToPipelineTransformer implements Transformer<WorkflowDefinitionScript, WorkflowPipeline> {

    /**
     * The input which says the most about a command, by the type of the command.
     */
    static final Map<String, String> DETAIL_INPUTS = Map.ofEntries(
        entry(ApprovalCommand.PREFIX, "message"),
        entry(DatabaseCommand.PREFIX, "url"),
        entry(GroovyCommand.PREFIX, "method"),
        entry(HttpRequestCommand.PREFIX, "url"),
        entry(JavaCommand.PREFIX, "method"),
        entry(MailCommand.PREFIX, "to"),
        entry(PrintCommand.PREFIX, "keys"),
        entry(ReportCommand.PREFIX, "title"),
        entry(SleepCommand.PREFIX, "time"),
        entry(SshExecCommand.PREFIX, "host"),
        entry(SshShellCommand.PREFIX, "host"),
        entry(WaitUntilSubPassesCommand.PREFIX, "rounds"));

    private static final int MAX_DETAIL_LENGTH = 80;

    private static final String PASSWORD = "password";

    @Override
    public WorkflowPipeline transform(WorkflowDefinitionScript workflowDefinitionScript) {
        return ofNullable(workflowDefinitionScript)
            .map(script -> new WorkflowPipeline(toSteps(script.getCommands()), toSteps(script.getFinallyCommands())))
            .orElseGet(() -> new WorkflowPipeline(List.of(), List.of()));
    }

    private List<WorkflowPipeline.Step> toSteps(List<AbstractCommand> commands) {
        return ofNullable(commands).orElse(List.of()).stream()
            .map(this::toStep)
            .toList();
    }

    private WorkflowPipeline.Step toStep(AbstractCommand command) {
        final var type = findType(command);
        return new WorkflowPipeline.Step(ofNullable(command.getName()).orElse(StringUtils.EMPTY), type, findDetail(command, type),
            namesOf(command.getOutputs()), namesOf(command.getIfs()),
            command instanceof AbstractSubCommand subCommand ? toSteps(subCommand.getSubcommands()) : List.of());
    }

    private String findType(AbstractCommand command) {
        return ofNullable(CommandType.nameOf(command)).orElse(StringUtils.EMPTY);
    }

    private String findDetail(AbstractCommand command, String type) {
        final var inputName = type + "." + DETAIL_INPUTS.get(type);
        return ofNullable(command.getInputs()).orElse(Set.of()).stream()
            .filter(input -> inputName.equals(input.getName()))
            // any other evaluator computes the value while the workflow runs
            .filter(input -> input.getRuntimeEvaluator() == RuntimeEvaluator.PLAIN)
            .findFirst()
            .flatMap(input -> toText(input.getValue()))
            .map(StringUtils::normalizeSpace)
            // an address can carry its credentials
            .filter(text -> !StringUtils.containsIgnoreCase(text, PASSWORD))
            .map(text -> StringUtils.abbreviate(text, MAX_DETAIL_LENGTH))
            .orElse(StringUtils.EMPTY);
    }

    private Optional<String> toText(Object value) {
        return switch (value) {
            case String text -> of(text);
            case Number number -> of(number.toString());
            case List<?> items -> of(items.stream().flatMap(item -> toText(item).stream()).collect(joining(", ")));
            case null, default -> empty();
        };
    }

    private List<String> namesOf(Collection<ExpressionNameValue> items) {
        return ofNullable(items).orElse(List.of()).stream()
            .map(ExpressionNameValue::getName)
            .filter(StringUtils::isNotBlank)
            .toList();
    }

}
