package org.myworkflows.view.util;

import de.f0rce.ace.AceEditor;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.command.ApprovalCommand;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.command.DatabaseCommand;
import org.myworkflows.domain.command.GroovyCommand;
import org.myworkflows.domain.command.HttpRequestCommand;
import org.myworkflows.domain.command.JavaCommand;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.domain.command.MailCommand;
import org.myworkflows.domain.command.PrintCommand;
import org.myworkflows.domain.command.SleepCommand;
import org.myworkflows.domain.command.SshExecCommand;
import org.myworkflows.domain.command.SshShellCommand;
import org.myworkflows.domain.command.WaitUntilSubPassesCommand;
import org.myworkflows.holder.CommandTypeHolder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class EditorAutoCompleteUtil {

    private static final List<String> WORKFLOW_KEYWORDS = new ArrayList<>();
    private static final Map<String, List<String>> COMMAND_INPUTS_COMPLETER = new LinkedHashMap<>();

    static {
        WORKFLOW_KEYWORDS.addAll(List.of("class", "name", "value", "ifs", "inputs", "asserts", "outputs", "commands", "subcommands", "finallyCommands"));
        COMMAND_INPUTS_COMPLETER.put(ApprovalCommand.PREFIX, List.of("message", "timeout"));
        COMMAND_INPUTS_COMPLETER.put(DatabaseCommand.PREFIX, List.of("url", "query"));
        COMMAND_INPUTS_COMPLETER.put(MailCommand.PREFIX, List.of("from", "to", "cc", "bcc", "subject", "body", "props", "bodyType", "username", "password"));
        COMMAND_INPUTS_COMPLETER.put(GroovyCommand.PREFIX, List.of("script", "method"));
        COMMAND_INPUTS_COMPLETER.put(HttpRequestCommand.PREFIX, List.of("url", "method", "body", "headers", "timeout"));
        COMMAND_INPUTS_COMPLETER.put(JavaCommand.PREFIX, List.of("script", "method", "clazz", "sourceVersion", "targetVersion"));
        COMMAND_INPUTS_COMPLETER.put(LoopCommand.PREFIX, List.of("items", "item", "backoffPeriod"));
        COMMAND_INPUTS_COMPLETER.put(PrintCommand.PREFIX, List.of("keys"));
        COMMAND_INPUTS_COMPLETER.put(SleepCommand.PREFIX, List.of("time"));
        COMMAND_INPUTS_COMPLETER.put(SshExecCommand.PREFIX, List.of("host", "username", "password", "command", "port", "timeout"));
        COMMAND_INPUTS_COMPLETER.put(SshShellCommand.PREFIX, List.of("host", "username", "password", "commands", "port", "timeout"));
        COMMAND_INPUTS_COMPLETER.put(WaitUntilSubPassesCommand.PREFIX, List.of("rounds", "backoffPeriod"));
    }

    public static void apply(AceEditor editor) {
        final var commandTypes = CommandTypeHolder.INSTANCE.getAll();
        editor.addStaticWordCompleter(WORKFLOW_KEYWORDS, editor.getTranslation("workflow-development.auto-complete.workflow-keyword"), true);
        editor.addStaticWordCompleter(commandTypes.stream().map(CommandType::name).toList(),
            editor.getTranslation("workflow-development.auto-complete.command-class"), true);
        editor.addDynamicWordCompleter(findCommandInputs(commandTypes), ".", editor.getTranslation("workflow-development.auto-complete.command-input"), true);
    }

    /**
     * @return the inputs to offer after the name of a command type: the ones listed here for a command of the
     *     application, and the parameters of its method for a command brought by a library.
     */
    private static Map<String, List<String>> findCommandInputs(Collection<CommandType> commandTypes) {
        final var commandInputs = new LinkedHashMap<>(COMMAND_INPUTS_COMPLETER);
        commandTypes.stream()
            .filter(commandType -> !commandType.builtIn() && !commandType.inputs().isEmpty())
            .forEach(commandType -> commandInputs.put(commandType.name(), commandType.inputNames()));
        return commandInputs;
    }

}
