package org.myworkflows.domain;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ExpressionNameValueTest {

    @Test
    public void testConstructorSPelEvaluationForString() {
        final var expressionNameValue = new ExpressionNameValue("test", "'a'", RuntimeEvaluator.SPEL);
        assertEquals("a", expressionNameValue.evaluate(Map.of()));
    }

    @Test
    public void testConstructorSPelEvaluationForList() {
        final var expressionNameValue = new ExpressionNameValue("test", List.of("'a'", "'b'"), RuntimeEvaluator.SPEL);
        final var result = expressionNameValue.evaluate(Map.of());
        assertInstanceOf(List.class, result);
        assertEquals(2, ((List<?>) result).size());
    }

    @Test
    public void testConstructorSPelEvaluationForMap() {
        final var expressionNameValue = new ExpressionNameValue("test", Map.of("'a'", "'b'"), RuntimeEvaluator.SPEL);
        final var result = expressionNameValue.evaluate(Map.of());
        assertInstanceOf(Map.class, result);
        assertEquals(1, ((Map<?, ?>) result).size());
    }

    @Test
    public void testJsonSPelEvaluationForString() {
        final var expressionNameValue = toObject("""
            name: test
            value: "'a'"
            class: spel
            """, ExpressionNameValue.class);
        assertEquals("a", expressionNameValue.evaluate(Map.of()));
    }

    @Test
    public void testJsonSPelEvaluationForList() {
        final var expressionNameValue = toObject("""
            name: test
            value: ["'a'", "'b'"]
            class: spel
            """, ExpressionNameValue.class);
        final var result = expressionNameValue.evaluate(Map.of());
        assertInstanceOf(List.class, result);
        assertEquals(2, ((List<?>) result).size());
    }

    @Test
    public void testJsonSPelEvaluationForMap() {
        final var expressionNameValue = toObject("""
            name: test
            value:
                "'a'": "'b'"
            class: spel
            """, ExpressionNameValue.class);
        final var result = expressionNameValue.evaluate(Map.of());
        assertInstanceOf(Map.class, result);
        assertEquals(1, ((Map<?, ?>) result).size());
    }

}
