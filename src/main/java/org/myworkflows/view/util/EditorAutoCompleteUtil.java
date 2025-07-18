package org.myworkflows.view.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.command.DatabaseCommand;
import org.myworkflows.domain.command.GroovyCommand;
import org.myworkflows.domain.command.HttpRequestCommand;
import org.myworkflows.domain.command.JavaCommand;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.domain.command.MailCommand;
import org.myworkflows.domain.command.NothingCommand;
import org.myworkflows.domain.command.PrintCommand;
import org.myworkflows.domain.command.SleepCommand;
import org.myworkflows.domain.command.SshExecCommand;
import org.myworkflows.domain.command.SshShellCommand;
import org.myworkflows.domain.command.WaitUntilSubPassesCommand;
import org.myworkflows.view.component.editor.AceEditor;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class EditorAutoCompleteUtil {

    private static final List<String> WORKFLOW_KEYWORDS = new ArrayList<>();
    private static final List<String> COMMAND_CLASSES = new ArrayList<>();
    private static final Map<String, List<String>> COMMAND_INPUTS_COMPLETER = new LinkedHashMap<>();

    static {
        WORKFLOW_KEYWORDS.addAll(List.of("class", "name", "value", "ifs", "inputs", "asserts", "outputs", "commands", "subcommands", "finallyCommands"));
        COMMAND_CLASSES.addAll(List.of(DatabaseCommand.PREFIX, GroovyCommand.PREFIX, HttpRequestCommand.PREFIX, JavaCommand.PREFIX,
            LoopCommand.PREFIX, MailCommand.PREFIX, NothingCommand.PREFIX, PrintCommand.PREFIX, SleepCommand.PREFIX,
            SshExecCommand.PREFIX, SshShellCommand.PREFIX, WaitUntilSubPassesCommand.PREFIX));
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
        editor.addStaticWordCompleter(WORKFLOW_KEYWORDS, editor.getTranslation("workflow-development.auto-complete.workflow-keyword"), true);
        editor.addStaticWordCompleter(COMMAND_CLASSES, editor.getTranslation("workflow-development.auto-complete.command-class"), true);
        editor.addDynamicWordCompleter(COMMAND_INPUTS_COMPLETER, ".", editor.getTranslation("workflow-development.auto-complete.command-input"), true);
    }

}
