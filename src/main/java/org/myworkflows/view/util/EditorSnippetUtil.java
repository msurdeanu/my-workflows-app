package org.myworkflows.view.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.f0rce.ace.AceEditor;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.config.SnippetConfig;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.holder.CommandTypeHolder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;

/**
 * The snippets of the commands: one per command type, with the inputs a command of that type cannot run without. They
 * are written from what {@link CommandType} reads from the commands, so a command brought by a library has its
 * snippet too - and so does a new command of the application, without anything to add here.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class EditorSnippetUtil {

    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();

    private static final String NAME_HINT = "name";

    private static final String CLASS_HINT = "class";

    /**
     * Registers the snippets of the workflow editor. It has to be called once Ace is ready, as the snippets are kept by
     * Ace and not by the component.
     */
    public static void apply(AceEditor editor) {
        editor.getElement().executeJs(SnippetConfig.JS_CODE, toJson(CommandTypeHolder.INSTANCE.getAll()));
    }

    /**
     * @return the snippets of the given command types as the JSON array Ace registers: a snippet is offered under the
     *     name of its command type, which is also what expands it with Tab.
     */
    public static String toJson(Collection<CommandType> commandTypes) {
        final var snippets = commandTypes.stream()
            .map(commandType -> Map.of("name", commandType.name(), "tabTrigger", commandType.name(), "content", toSnippet(commandType)))
            .toList();
        try {
            return JSON_MAPPER.writeValueAsString(snippets);
        } catch (JsonProcessingException exception) {
            throw new IllegalStateException(exception);
        }
    }

    /**
     * @return a command of the given type as an Ace snippet. It starts on a line of its own, a tab stands for one level
     *     of indentation and every value to fill in is a tab stop, which shows what is expected there until it is typed
     *     over: {@code ${2:Number}}.
     */
    public static String toSnippet(CommandType commandType) {
        final var tabStop = new AtomicInteger();
        return "\n" + render(commandType, "\t", EditorSnippetUtil::escape, hint -> "${" + tabStop.incrementAndGet() + ":" + escape(hint) + "}");
    }

    /**
     * @return the same command as plain YAML, to be read instead of filled in: what is expected stands between angle
     *     brackets.
     */
    public static String toExample(CommandType commandType) {
        return render(commandType, "  ", UnaryOperator.identity(), hint -> "<" + hint + ">");
    }

    private static String render(CommandType commandType, String indent, UnaryOperator<String> literal, UnaryOperator<String> placeholder) {
        final var lines = new ArrayList<String>();
        lines.add("- name: " + placeholder.apply(NAME_HINT));
        lines.add(indent + "class: " + literal.apply(commandType.name()));
        final var requiredInputs = commandType.requiredInputs();
        if (!requiredInputs.isEmpty()) {
            lines.add(indent + "inputs:");
        }
        requiredInputs.forEach(input -> {
            lines.add(indent.repeat(2) + "- name: " + literal.apply(commandType.name() + "." + input.name()));
            lines.add(indent.repeat(3) + "value: " + placeholder.apply(input.type()));
        });
        if (commandType.hasSubcommands()) {
            // the schema asks for the commands such a command runs, so the first of them is started as well
            lines.add(indent + "subcommands:");
            lines.add(indent.repeat(2) + "- name: " + placeholder.apply(NAME_HINT));
            lines.add(indent.repeat(3) + "class: " + placeholder.apply(CLASS_HINT));
        }
        return String.join("\n", lines);
    }

    /**
     * Ace reads a dollar as the start of a tab stop and a closing brace as its end, wherever they are.
     */
    private static String escape(String text) {
        return text.replace("\\", "\\\\").replace("$", "\\$").replace("}", "\\}");
    }

}
