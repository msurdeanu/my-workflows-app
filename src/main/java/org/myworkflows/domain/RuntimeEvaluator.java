package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.codehaus.janino.ExpressionEvaluator;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.util.HashMap;
import java.util.Map;

import static java.util.Arrays.stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
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

    JAVA("java") {
        @Override
        public Object evaluate(String expression, Map<String, Object> variables) {
            try {
                final var expressionEvaluator = new ExpressionEvaluator();
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
                expressionEvaluator.cook(expression);
                return expressionEvaluator.evaluate(parameterObjects);
            } catch (Exception e) {
                throw new WorkflowRuntimeException(e);
            }
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

    @JsonValue
    private final String type;

    @JsonCreator
    public static RuntimeEvaluator of(String type) {
        return ALL_VALUES.getOrDefault(type, PLAIN);
    }

    public abstract Object evaluate(String expression, Map<String, Object> variables);

}
