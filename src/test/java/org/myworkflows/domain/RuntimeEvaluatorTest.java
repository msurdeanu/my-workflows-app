package org.myworkflows.domain;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.myworkflows.domain.RuntimeEvaluator.GROOVY;
import static org.myworkflows.domain.RuntimeEvaluator.JAVA;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class RuntimeEvaluatorTest {

    @Test
    public void testSimpleGroovyExpression() {
        assertEquals(3, GROOVY.evaluate("a + b", Map.of("a", 1, "b", 2)));
    }

    @Test
    public void testSimpleJavaExpression() {
        assertEquals(3, JAVA.evaluate("a + b", Map.of("a", 1, "b", 2)));
    }

    @Test
    public void testThroughCacheJavaExpression() {
        final var cache = new WorkflowRunCache();
        cache.put("a", 1);
        cache.put("b", 2);
        assertEquals(false, JAVA.evaluate("cache.get(\"a\", Integer.class) == cache.get(\"b\", Integer.class)",
                Map.of("cache", cache)));
    }

}
