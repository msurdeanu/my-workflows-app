package org.myworkflows.domain.command;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.Getter;
import org.myworkflows.domain.ExecutionCache;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.OptionalParam;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static java.util.Arrays.stream;
import static java.util.Optional.ofNullable;
import static java.util.stream.IntStream.range;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "@type")
@JsonSubTypes({
    @JsonSubTypes.Type(value = GroovyCommand.class, name = "groovy"),
    @JsonSubTypes.Type(value = HttpRequestCommand.class, name = "httpRequest"),
    @JsonSubTypes.Type(value = NothingCommand.class, name = "nothing"),
    @JsonSubTypes.Type(value = PrintCommand.class, name = "print"),
    @JsonSubTypes.Type(value = SleepCommand.class, name = "sleep"),
    @JsonSubTypes.Type(value = SshExecCommand.class, name = "sshExec"),
    @JsonSubTypes.Type(value = SshShellCommand.class, name = "sshShell"),
    @JsonSubTypes.Type(value = WaitUntilSubPassesCommand.class, name = "waitUntilSubPasses")
})
@JsonIgnoreProperties(ignoreUnknown = true)
public abstract class AbstractCommand {

    private static final String EXECUTION_CACHE = "executionCache";

    private static final String OUTPUT = "output";

    @Getter
    @JsonProperty
    private String name;

    @JsonProperty("ifs")
    @JsonDeserialize(as = LinkedHashSet.class)
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private Set<ExpressionNameValue> ifs = Set.of();

    @JsonProperty("inputs")
    @JsonDeserialize(as = LinkedHashSet.class)
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private Set<ExpressionNameValue> inputs = Set.of();

    @JsonProperty("asserts")
    @JsonDeserialize(as = LinkedHashSet.class)
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private Set<ExpressionNameValue> asserts = Set.of();

    @JsonProperty("outputs")
    @JsonDeserialize(as = LinkedHashSet.class)
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private Set<ExpressionNameValue> outputs = Set.of();

    public final void run(ExecutionContext executionContext) {
        if (!runIfs(executionContext.getCache())) {
            return;
        }

        runInputs(executionContext.getCache());
        stream(getClass().getDeclaredMethods())
                .filter(method -> method.isAnnotationPresent(ExecutionMethod.class))
                .findFirst()
                .ifPresent(method -> runMethodWithAssertsAndOutputs(method, executionContext));
    }

    public boolean runIfs(ExecutionCache executionCache) {
        return ifs.stream().allMatch(item -> Boolean.TRUE.equals(item.evaluate(Map.of(EXECUTION_CACHE, executionCache))));
    }

    private void runInputs(ExecutionCache executionCache) {
        inputs.forEach(input -> executionCache.put(input.getName(), input.evaluate(Map.of(EXECUTION_CACHE, executionCache))));
    }

    private void runMethodWithAssertsAndOutputs(Method method, ExecutionContext executionContext) {
        runMethod(method, executionContext).ifPresent(output -> {
            runAsserts(output, executionContext.getCache());
            runOutputs(output, executionContext.getCache());
        });
    }

    private Optional<Object> runMethod(Method method, ExecutionContext executionContext) {
        try {
            return ofNullable(method.invoke(this, resolveParameters(method, executionContext)));
        } catch (IllegalAccessException | InvocationTargetException exception) {
            throw new WorkflowRuntimeException(exception);
        }
    }

    private Object[] resolveParameters(Method method, ExecutionContext executionContext) {
        final var parameters = method.getParameters();
        final var resolvedParameters = new Object[parameters.length];
        range(0, parameters.length)
                .forEach(index -> resolvedParameters[index] = resolveParameter(parameters[index], executionContext));
        return resolvedParameters;
    }

    private Object resolveParameter(Parameter parameter, ExecutionContext executionContext) {
        final var parameterIsMandatory = ofNullable(parameter.getDeclaredAnnotation(OptionalParam.class)).isEmpty();
        final var parameterType = parameter.getType();
        final var parameterizedType = parameter.getParameterizedType();
        if (parameterizedType instanceof ParameterizedType && List.class.equals(parameterType)) {
            final var actualTypeArguments = ((ParameterizedType) parameterizedType).getActualTypeArguments();
            return executionContext.getCache()
                    .lookupList(parameter.getName(), (Class<?>) actualTypeArguments[0], parameterIsMandatory);
        } else if (ExecutionContext.class.equals(parameterType)) {
            return executionContext;
        } else {
            return executionContext.getCache().lookup(parameter.getName(), parameterType, parameterIsMandatory);
        }
    }

    private void runAsserts(Object output, ExecutionCache executionCache) {
        asserts.stream()
                .filter(assertion -> !Boolean.TRUE.equals(assertion.evaluate(Map.of(EXECUTION_CACHE, executionCache, OUTPUT, output))))
                .findFirst()
                .ifPresent(assertion -> {
                    throw new WorkflowRuntimeException("Assertion name '" + assertion.getName() + "' with body '" + assertion.getValue() + "' failed.");
                });
    }

    private void runOutputs(Object output, ExecutionCache executionCache) {
        outputs.forEach(out -> executionCache.put(out.getName(), out.evaluate(Map.of(EXECUTION_CACHE, executionCache, OUTPUT, output))));
    }

}
