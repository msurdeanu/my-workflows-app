package org.myworkflows.domain;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ExpressionNameValueTest {

    @Test
    public void testSPelEvaluationForString() {
        final var expressionNameValue = fromJsonToObject("{\"name\":\"test\",\"value\":\"'a'\",\"@type\":\"spel\"}",
                ExpressionNameValue.class);
        assertEquals("a", expressionNameValue.evaluate(Map.of()));
    }

    @Test
    public void testSPelEvaluationForList() {
        final var expressionNameValue = fromJsonToObject("{\"name\":\"test\",\"value\":[\"'a'\", \"'b'\"],\"@type\":\"spel\"}",
                ExpressionNameValue.class);
        final var result = expressionNameValue.evaluate(Map.of());
        assertInstanceOf(List.class, result);
        assertEquals(2, ((List<?>) result).size());
    }

    @Test
    public void testSPelEvaluationForMap() {
        final var expressionNameValue = fromJsonToObject("{\"name\":\"test\",\"value\":{\"'a'\":\"'b'\"},\"@type\":\"spel\"}",
                ExpressionNameValue.class);
        final var result = expressionNameValue.evaluate(Map.of());
        assertInstanceOf(Map.class, result);
        assertEquals(1, ((Map<?, ?>) result).size());
    }

}
