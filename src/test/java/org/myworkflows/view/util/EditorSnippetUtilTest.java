package org.myworkflows.view.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.domain.command.EchoCommand;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.domain.command.NothingCommand;
import org.myworkflows.domain.command.SleepCommand;
import org.myworkflows.domain.command.SshExecCommand;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.holder.CommandTypeHolder;
import org.myworkflows.serializer.SerializerFactory;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class EditorSnippetUtilTest {

    @Test
    public void whenCommandHasRequiredInputsThenItsSnippetAsksForEachOfThem() {
        // when
        final var snippet = EditorSnippetUtil.toSnippet(CommandType.of(SshExecCommand.class, true));

        // then the optional inputs, the port and the timeout, are left out
        assertEquals("""

            - name: ${1:name}
            \tclass: sshExec
            \tinputs:
            \t\t- name: sshExec.host
            \t\t\tvalue: ${2:String}
            \t\t- name: sshExec.command
            \t\t\tvalue: ${3:String}
            \t\t- name: sshExec.username
            \t\t\tvalue: ${4:String}
            \t\t- name: sshExec.password
            \t\t\tvalue: ${5:String}""", snippet);
    }

    @Test
    public void whenCommandHasNoRequiredInputThenItsSnippetHasNoInputs() {
        assertEquals("\n- name: ${1:name}\n\tclass: nothing", EditorSnippetUtil.toSnippet(CommandType.of(NothingCommand.class, true)));
    }

    @Test
    public void whenCommandRunsSubcommandsThenItsSnippetStartsTheFirstOfThem() {
        // when
        final var snippet = EditorSnippetUtil.toSnippet(CommandType.of(LoopCommand.class, true));

        // then
        assertEquals("""

            - name: ${1:name}
            \tclass: loop
            \tinputs:
            \t\t- name: loop.items
            \t\t\tvalue: ${2:List<Object>}
            \tsubcommands:
            \t\t- name: ${3:name}
            \t\t\tclass: ${4:class}""", snippet);
    }

    @Test
    public void whenCommandIsBroughtByALibraryThenItHasASnippetToo() {
        assertEquals("\n- name: ${1:name}\n\tclass: echo\n\tinputs:\n\t\t- name: echo.text\n\t\t\tvalue: ${2:String}",
            EditorSnippetUtil.toSnippet(CommandType.of(EchoCommand.class, false)));
    }

    @Test
    public void whenNameWouldBeReadAsATabStopThenItIsEscaped() {
        // when
        final var snippet = EditorSnippetUtil.toSnippet(CommandType.of(DollarCommand.class, false));

        // then
        assertTrue(snippet.contains("- name: dollar.price\\$\n"));
    }

    @Test
    public void whenExampleIsAskedThenTheSameCommandIsWrittenAsPlainYaml() {
        // when
        final var example = EditorSnippetUtil.toExample(CommandType.of(SleepCommand.class, true));

        // then
        assertEquals("""
            - name: <name>
              class: sleep
              inputs:
                - name: sleep.time
                  value: <Number>""", example);
    }

    @Test
    public void whenExampleIsFilledInThenItIsACommandTheApplicationReads() {
        for (var commandType : CommandTypeHolder.INSTANCE.getAll()) {
            final var script = "commands:\n" + EditorSnippetUtil.toExample(commandType).indent(2)
                .replace("<name>", "a name")
                .replace("<class>", "nothing")
                .replaceAll("<[^>\n]+>", "a value");
            assertDoesNotThrow(() -> SerializerFactory.toObject(script, WorkflowDefinitionScript.class), commandType.name());
        }
    }

    @Test
    public void whenSnippetsAreHandedOverToAceThenEachIsOfferedUnderTheNameOfItsCommand() throws JsonProcessingException {
        // given
        final var sleepType = CommandType.of(SleepCommand.class, true);

        // when
        final var json = EditorSnippetUtil.toJson(List.of(sleepType, CommandType.of(NothingCommand.class, true)));

        // then
        final var snippets = new ObjectMapper().readValue(json, new TypeReference<List<Map<String, String>>>() {
        });
        assertEquals(2, snippets.size());
        assertEquals(Map.of("name", "sleep", "tabTrigger", "sleep", "content", EditorSnippetUtil.toSnippet(sleepType)), snippets.getFirst());
        assertEquals("nothing", snippets.getLast().get("name"));
    }

    public static final class DollarCommand extends AbstractCommand {

        @ExecutionMethod(prefix = "dollar")
        public void dollar(@ExecutionParam String price$) {
            // nothing to do
        }

    }

}
