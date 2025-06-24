package org.myworkflows.serializer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.exception.WorkflowRuntimeException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SerializerFactoryTest {

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
