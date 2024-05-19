package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.util.HashMap;
import java.util.Map;

import static java.util.Arrays.stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public enum RuntimeEvaluator {

    GROOVY("groovy") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables) {
            final var binding = new Binding();
            variables.forEach(binding::setProperty);
            final var groovyShell = new GroovyShell(binding);
            return groovyShell.evaluate(expression);
        }
    },

    PLAIN("plain") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables) {
            return expression;
        }
    },

    SPEL("spel") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables) {
            final var parsedExpression = new SpelExpressionParser().parseExpression(expression);
            final var context = new StandardEvaluationContext();
            context.setVariables(variables);
            return parsedExpression.getValue(context);
        }
    };

    private static final Map<String, RuntimeEvaluator> ALL_VALUES = new HashMap<>(3);

    static {
        stream(RuntimeEvaluator.values()).forEach(item -> ALL_VALUES.put(item.getType(), item));
    }

    @Getter
    @JsonValue
    private final String type;

    @JsonCreator
    public static RuntimeEvaluator of(String type) {
        return ALL_VALUES.getOrDefault(type, PLAIN);
    }

    public abstract Object evaluate(String expression, Map<String, Object> variables);

}
