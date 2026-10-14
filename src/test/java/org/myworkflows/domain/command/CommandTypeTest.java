package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class CommandTypeTest {

    @Test
    public void whenClassIsACommandThenItsNameAndItsInputsAreReadFromItsMethod() {
        // when
        final var sleepType = CommandType.of(SleepCommand.class, true);
        final var loopType = CommandType.of(LoopCommand.class, true);
        final var echoType = CommandType.of(EchoCommand.class, false);

        // then
        assertEquals("sleep", sleepType.name());
        assertEquals(List.of(new CommandInput("time", "Number", true, "")), sleepType.inputs());
        assertEquals("long", sleepType.output());
        assertTrue(sleepType.builtIn());
        assertFalse(sleepType.hasSubcommands());
        // the run is handed over to a command, so it is not one of its inputs
        assertEquals(List.of(new CommandInput("items", "List<Object>", true, ""), new CommandInput("backoffPeriod", "Number", false, "1000")),
            loopType.inputs());
        assertTrue(loopType.hasSubcommands());
        assertEquals("echo", echoType.name());
        assertEquals(List.of("text", "times"), echoType.inputNames());
        assertEquals(List.of("text"), echoType.requiredInputs().stream().map(CommandInput::name).toList());
        assertFalse(echoType.builtIn());
    }

    @Test
    public void whenInputOrOutputHasTypeArgumentsThenTheyAreToldWithoutTheirPackages() {
        // when
        final var httpRequestType = CommandType.of(HttpRequestCommand.class, true);
        final var headers = httpRequestType.inputs().get(3);

        // then
        assertEquals(new CommandInput("headers", "Map<String, String>", false, ":of"), headers);
        assertTrue(headers.hasDefaultValue());
        assertFalse(httpRequestType.inputs().get(2).hasDefaultValue());
        assertEquals("ResponseEntity<String>", httpRequestType.output());
        assertTrue(httpRequestType.hasOutput());
    }

    @Test
    public void whenCommandReturnsNothingThenItHasNoOutput() {
        // when
        final var nothingType = CommandType.of(NothingCommand.class, true);

        // then
        assertFalse(nothingType.hasOutput());
        assertEquals(List.of(), nothingType.inputs());
        assertEquals(List.of(), nothingType.requiredInputs());
    }

    @Test
    public void whenClassCannotBeRunAsACommandThenTheReasonIsTold() {
        // when and then
        assertTrue(assertThrows(WorkflowRuntimeException.class, () -> CommandType.of(String.class, false))
            .getMessage().contains("extends"));
        assertTrue(assertThrows(WorkflowRuntimeException.class, () -> CommandType.of(AbstractSubCommand.class, false))
            .getMessage().contains("abstract"));
        assertTrue(assertThrows(WorkflowRuntimeException.class, () -> CommandType.of(NoMethodCommand.class, false))
            .getMessage().contains("@ExecutionMethod"));
        assertTrue(assertThrows(WorkflowRuntimeException.class, () -> CommandType.of(TwoMethodsCommand.class, false))
            .getMessage().contains("@ExecutionMethod"));
        assertTrue(assertThrows(WorkflowRuntimeException.class, () -> CommandType.of(BadNameCommand.class, false))
            .getMessage().contains("'my.command'"));
    }

    @Test
    public void whenCommandIsGivenThenTheNameOfItsTypeIsFound() {
        // when and then
        assertEquals("sleep", CommandType.nameOf(new SleepCommand()));
        assertEquals("echo", CommandType.nameOf(new EchoCommand()));
    }

    public static final class NoMethodCommand extends AbstractCommand {

    }

    public static final class TwoMethodsCommand extends AbstractCommand {

        @ExecutionMethod(prefix = "first")
        public void first() {
            // nothing to do
        }

        @ExecutionMethod(prefix = "second")
        public void second() {
            // nothing to do
        }

    }

    public static final class BadNameCommand extends AbstractCommand {

        @ExecutionMethod(prefix = "my.command")
        public void execute() {
            // nothing to do
        }

    }

}
