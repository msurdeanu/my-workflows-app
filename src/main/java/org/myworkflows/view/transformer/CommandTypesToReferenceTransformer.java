package org.myworkflows.view.transformer;

import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.command.CommandInput;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.view.util.EditorSnippetUtil;

import java.util.Collection;
import java.util.function.BiFunction;

/**
 * Writes the reference of the command types as a Markdown page: what each of them does, its inputs, its output and an
 * example. All of it is read from the commands themselves, except the few words on what a command of the application
 * does, which are the ones the pipeline of a read-only link shows.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@RequiredArgsConstructor
public final class CommandTypesToReferenceTransformer implements Transformer<Collection<CommandType>, String> {

    private static final String PREFIX = "doc-pages.command-reference.";

    private static final String LINE = "\n";

    private static final String PARAGRAPH = "\n\n";

    /**
     * Gives the translation of a key, with its parameters.
     */
    private final BiFunction<String, Object[], String> translator;

    @Override
    public String transform(Collection<CommandType> commandTypes) {
        final var markdown = new StringBuilder(translate(PREFIX + "intro")).append(PARAGRAPH);
        commandTypes.forEach(commandType -> appendCommandType(markdown, commandType));
        return markdown.toString();
    }

    private void appendCommandType(StringBuilder markdown, CommandType commandType) {
        final var name = commandType.name();
        if (commandType.builtIn()) {
            markdown.append("## ").append(code(name)).append(" - ").append(translate("workflow-pipeline.command." + name + ".label")).append(PARAGRAPH);
            markdown.append(translate("workflow-pipeline.command." + name + ".text"));
        } else {
            // a command brought by a library has no translation of its own
            markdown.append("## ").append(code(name)).append(PARAGRAPH);
            markdown.append(translate("workflow-pipeline.plugin-command.text"));
        }
        if (commandType.hasSubcommands()) {
            markdown.append(' ').append(translate(PREFIX + "subcommands"));
        }
        markdown.append(PARAGRAPH);

        appendInputs(markdown, commandType);
        markdown.append(commandType.hasOutput() ? translate(PREFIX + "output", code(commandType.output())) : translate(PREFIX + "no-output"))
            .append(PARAGRAPH);
        markdown.append(translate(PREFIX + "example")).append(PARAGRAPH);
        markdown.append("```yaml").append(LINE).append(EditorSnippetUtil.toExample(commandType)).append(LINE).append("```").append(PARAGRAPH);
    }

    private void appendInputs(StringBuilder markdown, CommandType commandType) {
        if (commandType.inputs().isEmpty()) {
            markdown.append(translate(PREFIX + "no-input")).append(PARAGRAPH);
            return;
        }
        appendRow(markdown, translate(PREFIX + "input.column"), translate(PREFIX + "type.column"), translate(PREFIX + "required.column"),
            translate(PREFIX + "default.column"));
        appendRow(markdown, ":--", ":--", ":--", ":--");
        commandType.inputs().forEach(input -> appendRow(markdown, code(commandType.name() + "." + input.name()), code(input.type()),
            translate(PREFIX + (input.required() ? "required.yes" : "required.no")), findDefaultValue(input)));
        markdown.append(LINE);
    }

    private void appendRow(StringBuilder markdown, String... cells) {
        markdown.append("| ").append(String.join(" | ", cells)).append(" |").append(LINE);
    }

    private String findDefaultValue(CommandInput input) {
        return input.hasDefaultValue() ? code(input.defaultValue()) : "";
    }

    /**
     * @return the text as code. A pipe would end the cell of a table, even between the backticks.
     */
    private String code(String text) {
        return "`" + text.replace("|", "\\|") + "`";
    }

    private String translate(String key, Object... params) {
        return translator.apply(key, params);
    }

}
