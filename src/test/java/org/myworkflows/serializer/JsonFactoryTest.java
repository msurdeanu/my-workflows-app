package org.myworkflows.serializer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.command.LoopCommand;
import org.myworkflows.exception.WorkflowRuntimeException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class JsonFactoryTest {

    @Test
    public void testAllMethodsExposed() {
        // given
        final var expressionNameValue = new ExpressionNameValue("A", "B", RuntimeEvaluator.PLAIN);
        // when & then
        final var expressionNameValueAsString = JsonFactory.toString(expressionNameValue, "N/A");
        assertEquals("{\"name\":\"A\",\"value\":\"B\"}", expressionNameValueAsString);
        assertNotEquals("{\"name\":\"A\",\"value\":\"B\"}", JsonFactory.toPrettyString(expressionNameValue, "N/A"));
        assertNotEquals("{\"name\":\"A\",\"value\":\"B\"}", JsonFactory.toPrettyString("{\"name\":\"A\",\"value\":\"B\"}", "N/A"));
        assertDoesNotThrow(() -> JsonFactory.fromJsonToObject(expressionNameValueAsString, ExpressionNameValue.class));
        assertThrows(WorkflowRuntimeException.class, () -> JsonFactory.fromJsonToObject(expressionNameValueAsString, LoopCommand.class));
    }

}
