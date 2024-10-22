package org.myworkflows.converter;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowDefinitionScriptToStringConverterTest {

    @Test
    @SuppressWarnings("checkstyle:linelength")
    public void whenConverterIsUsedEverythingWorksAsExpected() {
        // given
        final var data =
            "{\"commands\":[{\"@type\":\"sleep\",\"name\":\"Sleep command\",\"inputs\":[{\"name\":\"sleep.time\",\"value\":1000}]},{\"@type\":\"print\",\"name\":\"Print command\",\"inputs\":[{\"name\":\"print.keys\",\"value\":[\"sleep.time\"]}]}]}";
        final var converter = new WorkflowDefinitionScriptToStringConverter();

        // when & then
        final var workflowDefinitionScript = converter.convertToEntityAttribute(data);
        assertNotNull(workflowDefinitionScript);
        assertEquals(2, workflowDefinitionScript.getCommands().size());
        assertEquals("Sleep command", workflowDefinitionScript.getCommands().getFirst().getName());
        assertEquals("Print command", workflowDefinitionScript.getCommands().getLast().getName());
        assertEquals(0, workflowDefinitionScript.getFinallyCommands().size());
        final var convertedData = converter.convertToDatabaseColumn(workflowDefinitionScript);
        assertEquals(data, convertedData);
    }

}
