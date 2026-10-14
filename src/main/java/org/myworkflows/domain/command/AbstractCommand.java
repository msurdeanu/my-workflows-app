package org.myworkflows.domain.command;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonTypeIdResolver;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;

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
 * @since 1.0
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
// the class behind a "class" field is looked up among the command types known when a definition is read, so that a
// library can bring its own; the field is also handed over to the command, for the one which stands for an unknown type
@JsonTypeInfo(use = JsonTypeInfo.Id.CUSTOM, property = "class", visible = true)
@JsonTypeIdResolver(CommandTypeIdResolver.class)
@JsonIgnoreProperties(ignoreUnknown = true)
public abstract class AbstractCommand {

    private static final String WORKFLOW_CACHE = "cache";

    private static final String OUTPUT = "output";

    private static final String NULL_AS_STR = "null";

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

    public final void run(WorkflowRun workflowRun) {
        run(workflowRun, null);
    }

    public final void run(WorkflowRun workflowRun, Object innerObject) {
        if (!runIfs(workflowRun.getCache())) {
            return;
        }

        runInputs(workflowRun.getCache());
        stream(getClass().getDeclaredMethods())
            .filter(method -> method.isAnnotationPresent(ExecutionMethod.class))
            .findFirst()
            .ifPresent(method -> runMethodWithAssertsAndOutputs(method, workflowRun, innerObject));
    }

    private boolean runIfs(WorkflowRunCache workflowRunCache) {
        return ifs.stream().allMatch(item -> Boolean.TRUE.equals(item.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache))));
    }

    private void runInputs(WorkflowRunCache workflowRunCache) {
        inputs.forEach(input -> workflowRunCache.put(input.getName(), input.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache))));
    }

    private void runMethodWithAssertsAndOutputs(Method method, WorkflowRun workflowRun, Object innerObject) {
        runMethod(method, workflowRun).ifPresent(output -> {
            runAsserts(output, workflowRun.getCache());
            runOutputs(output, workflowRun.getCache(), innerObject);
        });
    }

    private Optional<Object> runMethod(Method method, WorkflowRun workflowRun) {
        return WorkflowRuntimeException.wrap(() -> ofNullable(method.invoke(this, resolveParameters(method, workflowRun))));
    }

    private Object[] resolveParameters(Method method, WorkflowRun workflowRun) {
        final var parameters = method.getParameters();
        final var executionMethod = method.getDeclaredAnnotation(ExecutionMethod.class);
        final var resolvedParameters = new Object[parameters.length];
        range(0, parameters.length).forEach(index -> resolvedParameters[index] = resolveParameter(parameters[index], executionMethod, workflowRun));
        if (workflowRun.isDebugModeEnabled()) {
            range(0, parameters.length)
                .filter(index -> !isBypassed(parameters[index]))
                .forEach(index -> {
                    final var cache = workflowRun.getCache();
                    final var parameterName = parameters[index].getName();
                    final var resolvedParameter = resolvedParameters[index];
                    cache.putAsDebug(name, executionMethod.prefix() + "." + parameterName + ">type",
                        ofNullable(resolvedParameter).map(item -> item.getClass().getSimpleName()).orElse(NULL_AS_STR));
                    cache.putAsDebug(name, executionMethod.prefix() + "." + parameterName + ">value",
                        resolvedParameter);
                });
        }
        return resolvedParameters;
    }

    private boolean isBypassed(Parameter parameter) {
        return ofNullable(parameter.getDeclaredAnnotation(ExecutionParam.class))
            .map(ExecutionParam::bypassed)
            .orElse(false);
    }

    private Object resolveParameter(Parameter parameter, ExecutionMethod executionMethod, WorkflowRun workflowRun) {
        final var optionalExecutionParam = ofNullable(parameter.getDeclaredAnnotation(ExecutionParam.class));
        final boolean paramIsMandatory = optionalExecutionParam.map(ExecutionParam::required).orElse(false);
        final var paramDefaultValue = optionalExecutionParam.map(ExecutionParam::defaultValue).orElse(null);
        final var parameterType = parameter.getType();
        final var parameterizedType = parameter.getParameterizedType();
        if (parameterizedType instanceof ParameterizedType && List.class.equals(parameterType)) {
            final var actualTypeArguments = ((ParameterizedType) parameterizedType).getActualTypeArguments();
            return workflowRun.getCache()
                .lookupList(executionMethod.prefix() + "." + parameter.getName(), (Class<?>) actualTypeArguments[0], paramIsMandatory, paramDefaultValue);
        } else if (WorkflowRun.class.equals(parameterType)) {
            return workflowRun;
        } else {
            return workflowRun.getCache().lookup(executionMethod.prefix() + "." + parameter.getName(), parameterType, paramIsMandatory, paramDefaultValue);
        }
    }

    private void runAsserts(Object output, WorkflowRunCache workflowRunCache) {
        asserts.stream()
            .filter(assertion -> !Boolean.TRUE.equals(assertion.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache, OUTPUT, output))))
            .findFirst()
            .ifPresent(assertion -> {
                throw new WorkflowRuntimeException("Assertion name '" + assertion.getName() + "' with body '" + assertion.getValue() + "' failed.");
            });
    }

    private void runOutputs(Object output, WorkflowRunCache workflowRunCache, Object innerObject) {
        ofNullable(innerObject).ifPresentOrElse(inner -> outputs.forEach(out -> workflowRunCache.putAsMap(out.getName(), inner,
                out.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache, OUTPUT, output)))),
            () -> outputs.forEach(out -> workflowRunCache.put(out.getName(), out.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache, OUTPUT, output)))));
    }

}
