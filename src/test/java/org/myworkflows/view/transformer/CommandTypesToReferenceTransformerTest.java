package org.myworkflows.view.transformer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.command.EchoCommand;
import org.myworkflows.domain.command.HttpRequestCommand;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.domain.command.NothingCommand;
import org.myworkflows.holder.CommandTypeHolder;
import org.myworkflows.provider.TranslationProvider;

import java.util.List;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class CommandTypesToReferenceTransformerTest {

    private final TranslationProvider translationProvider = new TranslationProvider();

    private final CommandTypesToReferenceTransformer transformer = new CommandTypesToReferenceTransformer(
        (key, params) -> translationProvider.getTranslation(key, Locale.ENGLISH, params));

    @Test
    public void whenCommandHasInputsThenEachOfThemIsARowOfItsTable() {
        // when
        final var markdown = transformer.transform(List.of(CommandType.of(HttpRequestCommand.class, true)));

        // then
        assertTrue(markdown.startsWith("Every type of command"));
        assertTrue(markdown.contains("""
            ## `httpRequest` - HTTP request

            Sends an HTTP request.

            | Input | Type | Required | Default |
            | :-- | :-- | :-- | :-- |
            | `httpRequest.url` | `String` | Yes |  |
            | `httpRequest.method` | `String` | No | `GET` |
            | `httpRequest.body` | `String` | No |  |
            | `httpRequest.headers` | `Map<String, String>` | No | `:of` |
            | `httpRequest.timeout` | `Long` | No | `60000` |

            Output: `ResponseEntity<String>`

            With its required inputs:

            ```yaml
            - name: <name>
              class: httpRequest
              inputs:
                - name: httpRequest.url
                  value: <String>
            ```
            """));
    }

    @Test
    public void whenCommandHasNeitherInputNorOutputThenBothAreSaid() {
        // when
        final var markdown = transformer.transform(List.of(CommandType.of(NothingCommand.class, true)));

        // then
        assertTrue(markdown.contains("## `nothing` - Inputs only\n\nRuns nothing: it only sets its inputs.\n\nIt takes no input.\n\n"
            + "It has no output, so its asserts and outputs are not evaluated.\n\n"));
        assertFalse(markdown.contains("| Input |"));
    }

    @Test
    public void whenCommandRunsSubcommandsThenTheReferenceSaysWhereTheyAreListed() {
        // when
        final var markdown = transformer.transform(List.of(CommandType.of(LoopCommand.class, true)));

        // then
        assertTrue(markdown.contains("Runs the commands below once for every item of a list. The commands it runs are listed under its `subcommands`.\n"));
        assertTrue(markdown.contains("  subcommands:\n    - name: <name>\n      class: <class>\n```"));
    }

    @Test
    public void whenCommandIsBroughtByALibraryThenItIsDescribedWithoutATranslationOfItsOwn() {
        // when
        final var markdown = transformer.transform(List.of(CommandType.of(EchoCommand.class, false)));

        // then
        assertTrue(markdown.contains("## `echo`\n\nRuns a command brought by a library.\n\n"));
        assertTrue(markdown.contains("| `echo.times` | `Number` | No | `1` |\n"));
        assertTrue(markdown.contains("Output: `String`\n"));
    }

    @Test
    public void whenEveryCommandOfTheApplicationIsDescribedThenNoTranslationIsMissing() {
        // given
        final var commandTypes = CommandTypeHolder.INSTANCE.getAll();

        // when
        final var markdown = transformer.transform(commandTypes);

        // then a missing translation would show up as its key
        assertFalse(markdown.contains("workflow-pipeline."));
        assertFalse(markdown.contains("doc-pages."));
        assertEquals(commandTypes.size(), markdown.split("\n## ", -1).length - 1);
    }

}
