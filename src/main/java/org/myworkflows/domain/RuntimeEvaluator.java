package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.codehaus.janino.ExpressionEvaluator;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.ParentClassLoaderHolder;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.expression.spel.support.StandardTypeLocator;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.lang.String.format;
import static java.util.Arrays.stream;
import static java.util.Optional.ofNullable;
import static org.myworkflows.util.StringReplacer.replace;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Getter
@RequiredArgsConstructor
public enum RuntimeEvaluator {

    GROOVY("groovy") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables, Pattern cacheAccessPattern) {
            final var newExpression = resolveCacheAccessPatterns(expression, cacheAccessPattern,
                "cache.get(\"%s\")", "cache.get(\"%s\", %s)");
            final var binding = new Binding();
            variables.forEach(binding::setProperty);
            return new GroovyShell(ParentClassLoaderHolder.INSTANCE.getClassLoader(), binding).evaluate(newExpression);
        }
    },

    JAVA("java") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables, Pattern cacheAccessPattern) {
            try {
                final var newExpression = resolveCacheAccessPatterns(expression, cacheAccessPattern,
                    "cache.get(\"%s\")", "cache.get(\"%s\", %s)");
                final var expressionEvaluator = new ExpressionEvaluator();
                expressionEvaluator.setParentClassLoader(ParentClassLoaderHolder.INSTANCE.getClassLoader());
                final var parameterNames = new String[variables.size()];
                final var parameterTypes = new Class<?>[variables.size()];
                final var parameterObjects = new Object[variables.size()];
                int index = 0;
                for (Map.Entry<String, Object> variable : variables.entrySet()) {
                    parameterNames[index] = variable.getKey();
                    parameterTypes[index] = variable.getValue().getClass();
                    parameterObjects[index] = variable.getValue();
                    index++;
                }
                expressionEvaluator.setParameters(parameterNames, parameterTypes);
                expressionEvaluator.cook(newExpression);
                return expressionEvaluator.evaluate(parameterObjects);
            } catch (Exception e) {
                throw new WorkflowRuntimeException(e);
            }
        }
    },

    PLAIN("plain") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables, Pattern cacheAccessPattern) {
            return expression;
        }
    },

    SPEL("spel") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables, Pattern cacheAccessPattern) {
            final var newExpression = resolveCacheAccessPatterns(expression, cacheAccessPattern,
                "#cache.get(\"%s\")", "#cache.get(\"%s\", T(%s))");
            final var parsedExpression = new SpelExpressionParser().parseExpression(newExpression);
            final var context = new StandardEvaluationContext();
            context.setTypeLocator(new StandardTypeLocator(ParentClassLoaderHolder.INSTANCE.getClassLoader()));
            context.setVariables(variables);
            return parsedExpression.getValue(context);
        }
    };

    private static final Map<String, RuntimeEvaluator> ALL_VALUES = new HashMap<>(4);

    static {
        stream(RuntimeEvaluator.values()).forEach(item -> ALL_VALUES.put(item.getType(), item));
    }

    @JsonValue
    private final String type;

    @JsonCreator
    public static RuntimeEvaluator of(String type) {
        return ALL_VALUES.getOrDefault(type, PLAIN);
    }

    private static String resolveCacheAccessPatterns(String expression, Pattern cacheAccessPattern, String formatWithoutType, String formatWithType) {
        final var newExpression = replace(expression, cacheAccessPattern, (Matcher matcher) -> {
            final var name = matcher.group(1);
            final var genericType = matcher.group(2);
            return ofNullable(genericType)
                .map(item -> format(formatWithType, name, item.substring(1)))
                .orElseGet(() -> format(formatWithoutType, name));
        });
        if (log.isDebugEnabled()) {
            log.debug("After resolving cache access patterns, expression '{}' translates into '{}'.", expression, newExpression);
        }
        return newExpression;
    }

    public abstract Object evaluate(String expression, Map<String, Object> variables, Pattern cacheAccessPattern);

}
