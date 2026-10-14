package org.myworkflows.serializer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.domain.command.SleepCommand;
import org.myworkflows.domain.command.UnknownCommand;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.CommandTypeHolder;
import org.myworkflows.util.ExceptionUtil;

import static java.util.stream.Collectors.joining;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SerializerFactoryTest {

    private static final String SCRIPT_WITH_UNKNOWN_COMMANDS = """
        commands:
        - class: sleep
          name: Wait a little
          inputs:
          - name: sleep.time
            value: 10
        - class: gone
          name: Brought by a library
          ifs:
          - name: Only when enabled
            value: cache.get("enabled")
            class: groovy
          inputs:
          - name: gone.text
            value: Hello
          outputs:
          - name: result
            value: output
            class: groovy
        - class: goneLoop
          name: Brought by a library too
          subcommands:
          - class: gone
            name: Inside
        """;

    @Test
    public void whenCommandIsOneOfTheApplicationThenItIsReadAndWrittenByItsName() {
        // given a script with one command of every type
        final var script = CommandTypeHolder.INSTANCE.getAll().stream()
            .map(commandType -> "- class: %s\n  name: A command\n%s".formatted(commandType.name(), commandType.hasSubcommands() ? "  subcommands: []\n" : ""))
            .collect(joining("", "commands:\n", ""));

        // when
        final var workflowDefinitionScript = SerializerFactory.toObject(script, WorkflowDefinitionScript.class);

        // then
        assertEquals(CommandTypeHolder.INSTANCE.getAll().stream().map(commandType -> (Object) commandType.type()).toList(),
            workflowDefinitionScript.getCommands().stream().map(command -> (Object) command.getClass()).toList());
        assertEquals(script, SerializerFactory.toString(workflowDefinitionScript, null));
    }

    @Test
    public void whenCommandTypeIsNotKnownThenContentIsNotRead() {
        // when and then
        final var exception = assertThrows(WorkflowRuntimeException.class,
            () -> SerializerFactory.toObject(SCRIPT_WITH_UNKNOWN_COMMANDS, WorkflowDefinitionScript.class));
        assertTrue(exception.getCause().getMessage().contains("Could not resolve type id 'gone'"));
        assertTrue(exception.getCause().getMessage().contains("waitUntilSubPasses"));
    }

    @Test
    public void whenUnknownCommandsAreKeptThenContentIsReadAndWrittenBackAsItWas() {
        // when
        final var workflowDefinitionScript = SerializerFactory.toObjectKeepingUnknownCommands(SCRIPT_WITH_UNKNOWN_COMMANDS,
            WorkflowDefinitionScript.class);

        // then the commands which are known are the ones they always were
        final var commands = workflowDefinitionScript.getCommands();
        assertInstanceOf(SleepCommand.class, commands.getFirst());
        final var unknownCommand = assertInstanceOf(UnknownCommand.class, commands.get(1));
        assertEquals("gone", unknownCommand.getType());
        assertEquals("Brought by a library", unknownCommand.getName());
        final var unknownCommandWithSubcommands = assertInstanceOf(UnknownCommand.class, commands.get(2));
        assertEquals(1, unknownCommandWithSubcommands.getSubcommands().size());
        // and nothing is lost when the script is saved again
        assertEquals(SCRIPT_WITH_UNKNOWN_COMMANDS, SerializerFactory.toString(workflowDefinitionScript, null));

        // and an unknown command fails only when it is run
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> unknownCommandWithSubcommands.run(new WorkflowRun()));
        assertTrue(ExceptionUtil.getMessageAndCause(exception).contains("Command class 'goneLoop' is not known: the library which brings it is probably not loaded."));
    }

    @Test
    public void testAllMethodsExposed() {
        // given
        final var expressionNameValue = new ExpressionNameValue("A", "B", RuntimeEvaluator.PLAIN);
        final var expectedNameValue = """
            name: A
            value: B
            """;
        // when and then
        final var expressionNameValueAsString = SerializerFactory.toString(expressionNameValue, "N/A");
        assertEquals(expectedNameValue, expressionNameValueAsString);
        assertEquals(expectedNameValue, SerializerFactory.toPrettyString("""
            name :     A
            value : B
            """, "N/A"));
        assertDoesNotThrow(() -> SerializerFactory.toObject(expressionNameValueAsString, ExpressionNameValue.class));
        assertThrows(WorkflowRuntimeException.class, () -> SerializerFactory.toObject(expressionNameValueAsString, LoopCommand.class));
    }

}
