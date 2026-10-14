package org.myworkflows.view.util;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class ScriptCommandUtilTest {

    private static final String DEFINITION = """
        # Nightly order export
        commands:
        - name: Read the open orders
          class: httpRequest
          inputs:
          - name: httpRequest.url
            value: https://shop.example/orders

        # the orders are counted one by one
        - name: Count the orders
          class: loop
          subcommands:
          - name: Count one order
            class: nothing
        - name: Print the outcome
          class: print

        finallyCommands:
        - name: Tell the team
          class: mail
        """;

    @Test
    public void whenDefinitionIsLocatedThenEveryCommandGetsTheLinesItIsWrittenOn() {
        // when
        final var commands = ScriptCommandUtil.locate(DEFINITION);

        // then a comment or an empty line belongs to the command above it
        assertEquals(List.of(
            new ScriptCommand(false, 0, "Read the open orders", 2, 8),
            new ScriptCommand(false, 1, "Count the orders", 9, 13),
            new ScriptCommand(false, 2, "Print the outcome", 14, 16),
            new ScriptCommand(true, 0, "Tell the team", 18, 19)), commands);
    }

    @Test
    public void whenRowBelongsToASubcommandThenTheCommandWhichRunsItIsFound() {
        // given
        final var commands = ScriptCommandUtil.locate(DEFINITION);

        // when and then
        assertEquals("Count the orders", ScriptCommandUtil.findByRow(commands, 12).map(ScriptCommand::name).orElse(null));
        assertEquals("Read the open orders", ScriptCommandUtil.findByRow(commands, 2).map(ScriptCommand::name).orElse(null));
        assertTrue(ScriptCommandUtil.findByRow(commands, 19).map(ScriptCommand::finalCommand).orElse(false));
        // neither the first comment nor the keys which list the commands belong to one of them
        assertEquals(Optional.empty(), ScriptCommandUtil.findByRow(commands, 0));
        assertEquals(Optional.empty(), ScriptCommandUtil.findByRow(commands, 1));
        assertEquals(Optional.empty(), ScriptCommandUtil.findByRow(commands, 17));
        assertEquals(Optional.empty(), ScriptCommandUtil.findByRow(commands, 40));
    }

    @Test
    public void whenCommandIsLookedUpByItsPositionThenFinalCommandsAreLeftOut() {
        // given
        final var commands = ScriptCommandUtil.locate(DEFINITION);

        // when and then
        assertEquals(Optional.of(new ScriptCommand(false, 0, "Read the open orders", 2, 8)), ScriptCommandUtil.findByIndex(commands, 0));
        assertEquals(14, ScriptCommandUtil.findByIndex(commands, 2).map(ScriptCommand::firstRow).orElse(-1));
        assertEquals(Optional.empty(), ScriptCommandUtil.findByIndex(commands, 3));
    }

    @Test
    public void whenRowsAreTurnedIntoPositionsThenOnlyTheCommandsARunCanPauseBeforeAreKept() {
        // given the rows of four breakpoints: two on the same command, one on a final command and one on no command
        final var commands = ScriptCommandUtil.locate(DEFINITION);

        // when and then
        assertEquals(Set.of(0, 1), ScriptCommandUtil.findIndexesByRows(commands, List.of(3, 9, 12, 18, 1)));
    }

    @Test
    public void whenDefinitionIndentsItsCommandsAndEndsWithoutALineBreakThenLastCommandEndsOnLastLine() {
        // given
        final var definition = "commands:\n  - name: First\n    class: nothing\n  - name: Second\n    class: nothing";

        // when and then
        assertEquals(List.of(new ScriptCommand(false, 0, "First", 1, 2), new ScriptCommand(false, 1, "Second", 3, 4)),
            ScriptCommandUtil.locate(definition));
    }

    @Test
    public void whenCommandIsAnAliasThenItHasNoLinesOfItsOwn() {
        // given a command which is written once and listed twice
        final var definition = """
            commands:
            - &ping
              name: Ping
              class: nothing
            - *ping
            - name: Done
              class: nothing
            """;

        // when
        final var commands = ScriptCommandUtil.locate(definition);

        // then the positions are kept, and no row leads to the alias
        assertEquals(List.of(new ScriptCommand(false, 0, "Ping", 1, 4), new ScriptCommand(false, 1, "Ping", -1, -1),
            new ScriptCommand(false, 2, "Done", 5, 6)), commands);
        assertFalse(commands.get(1).contains(4));
        assertEquals(Set.of(0), ScriptCommandUtil.findIndexesByRows(commands, List.of(4)));
    }

    @Test
    public void whenDefinitionCannotBeReadThenNoCommandIsFound() {
        assertEquals(List.of(), ScriptCommandUtil.locate(null));
        assertEquals(List.of(), ScriptCommandUtil.locate(""));
        assertEquals(List.of(), ScriptCommandUtil.locate("commands: [unclosed"));
        assertEquals(List.of(), ScriptCommandUtil.locate("- just\n- a list"));
        assertEquals(List.of(), ScriptCommandUtil.locate("commands: none"));
    }

}
