package org.myworkflows.view.util;

import de.f0rce.ace.AceEditor;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

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
    private static final List<String> COMMAND_TYPES = new ArrayList<>();
    private static final Map<String, List<String>> COMMAND_INPUTS_COMPLETER = new LinkedHashMap<>();

    static {
        WORKFLOW_KEYWORDS.addAll(List.of("@type", "name", "value", "ifs", "inputs", "asserts", "outputs", "commands", "subcommands", "finallyCommands"));
        COMMAND_TYPES.addAll(List.of("database", "groovy", "httpRequest", "java", "nothing", "print", "sleep", "sshExec", "sshShell", "waitUntilSubPasses"));
        COMMAND_INPUTS_COMPLETER.put("database", List.of("url", "query"));
        COMMAND_INPUTS_COMPLETER.put("groovy", List.of("scriptLines", "methodName"));
        COMMAND_INPUTS_COMPLETER.put("java", List.of("scriptLines", "methodName", "className"));
        COMMAND_INPUTS_COMPLETER.put("httpRequest", List.of("url", "method", "body", "headers", "connectionTimeout", "readTimeout"));
        COMMAND_INPUTS_COMPLETER.put("print", List.of("keys"));
        COMMAND_INPUTS_COMPLETER.put("sleep", List.of("time"));
        COMMAND_INPUTS_COMPLETER.put("sshExec", List.of("host", "username", "password", "command", "port", "timeout"));
        COMMAND_INPUTS_COMPLETER.put("sshShell", List.of("host", "username", "password", "commands", "port", "timeout"));
        COMMAND_INPUTS_COMPLETER.put("waitUntilSubPasses", List.of("iterations", "backoffPeriod"));
    }

    @SuppressWarnings("deprecation")
    public static void apply(AceEditor editor) {
        editor.setCustomAutocompletion(WORKFLOW_KEYWORDS, editor.getTranslation("workflow-development.auto-complete.workflow-keyword"), true);
        editor.setCustomAutocompletion(COMMAND_TYPES, editor.getTranslation("workflow-development.auto-complete.command-type"), true);
        editor.addDynamicAutocompletion(COMMAND_INPUTS_COMPLETER, ".", editor.getTranslation("workflow-development.auto-complete.command-input"), true);
    }

}
