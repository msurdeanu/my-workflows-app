package org.myworkflows.converter;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowParameterType;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ParameterTypeToStringConverterTest {

    @Test
    public void testConvertToDatabaseColumn() {
        final var parameterTypeToStringConverter = new ParameterTypeToStringConverter();

        assertNull(parameterTypeToStringConverter.convertToDatabaseColumn(null));
        assertEquals("str", parameterTypeToStringConverter.convertToDatabaseColumn(WorkflowParameterType.STR));
    }

    @Test
    public void testConvertToEntityAttribute() {
        final var parameterTypeToStringConverter = new ParameterTypeToStringConverter();

        assertNull(parameterTypeToStringConverter.convertToEntityAttribute(null));
        assertEquals(WorkflowParameterType.INT, parameterTypeToStringConverter.convertToEntityAttribute("int"));
    }

}
