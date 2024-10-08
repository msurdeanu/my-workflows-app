package org.myworkflows.domain;

import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.myworkflows.domain.RuntimeEvaluator.GROOVY;
import static org.myworkflows.domain.RuntimeEvaluator.JAVA;
import static org.myworkflows.domain.RuntimeEvaluator.SPEL;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class RuntimeEvaluatorTest {

    private static final Pattern CACHE_ACCESS_PATTERN = Pattern.compile("\\$\\(([a-zA-Z0-9_]+)(:[a-zA-Z0-9_.]+)?\\)");

    @Test
    public void testSimpleGroovyExpression() {
        assertEquals(3, GROOVY.evaluate("a + b", Map.of("a", 1, "b", 2), CACHE_ACCESS_PATTERN));
    }

    @Test
    public void testSimpleJavaExpression() {
        assertEquals(3, JAVA.evaluate("a + b", Map.of("a", 1, "b", 2), CACHE_ACCESS_PATTERN));
    }

    @Test
    public void testThroughCacheJavaExpression() {
        final var cache = new WorkflowRunCache();
        cache.put("a", 1);
        cache.put("b", 2);
        assertEquals(false, JAVA.evaluate("cache.get(\"a\", Integer.class) == cache.get(\"b\", Integer.class)",
                Map.of("cache", cache), CACHE_ACCESS_PATTERN));
    }

    @Test
    public void testSimpleAccessCachePatterns() {
        final var cache = new WorkflowRunCache();
        cache.put("a", 1);
        cache.put("b", 1);
        assertEquals(true, GROOVY.evaluate("$(a).equals($(b))", Map.of("cache", cache), CACHE_ACCESS_PATTERN));
        assertEquals(true, GROOVY.evaluate("$(a:Integer.class) == $(b:Integer.class)", Map.of("cache", cache), CACHE_ACCESS_PATTERN));
        assertEquals(true, JAVA.evaluate("$(a).equals($(b))", Map.of("cache", cache), CACHE_ACCESS_PATTERN));
        assertEquals(true, JAVA.evaluate("$(a:Integer.class) == $(b:Integer.class)", Map.of("cache", cache), CACHE_ACCESS_PATTERN));
        assertEquals(true, SPEL.evaluate("$(a) == $(b)", Map.of("cache", cache), CACHE_ACCESS_PATTERN));
        assertEquals(true, SPEL.evaluate("$(a:java.lang.Integer) == $(b:java.lang.Integer)", Map.of("cache", cache), CACHE_ACCESS_PATTERN));
    }

}
