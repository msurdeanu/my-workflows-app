package org.myworkflows.domain.command;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import lombok.Getter;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
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

    private static final String WORKFLOW_CACHE = "workflowCache";

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

    public final void run(WorkflowRun workflowRun) {
        if (!runIfs(workflowRun.getCache())) {
            return;
        }

        runInputs(workflowRun.getCache());
        stream(getClass().getDeclaredMethods())
                .filter(method -> method.isAnnotationPresent(ExecutionMethod.class))
                .findFirst()
                .ifPresent(method -> runMethodWithAssertsAndOutputs(method, workflowRun));
    }

    public boolean runIfs(WorkflowRunCache workflowRunCache) {
        return ifs.stream().allMatch(item -> Boolean.TRUE.equals(item.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache))));
    }

    private void runInputs(WorkflowRunCache workflowRunCache) {
        inputs.forEach(input -> workflowRunCache.put(input.getName(), input.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache))));
    }

    private void runMethodWithAssertsAndOutputs(Method method, WorkflowRun workflowRun) {
        runMethod(method, workflowRun).ifPresent(output -> {
            runAsserts(output, workflowRun.getCache());
            runOutputs(output, workflowRun.getCache());
        });
    }

    private Optional<Object> runMethod(Method method, WorkflowRun workflowRun) {
        try {
            return ofNullable(method.invoke(this, resolveParameters(method, workflowRun)));
        } catch (IllegalAccessException | InvocationTargetException exception) {
            throw new WorkflowRuntimeException(exception);
        }
    }

    private Object[] resolveParameters(Method method, WorkflowRun workflowRun) {
        final var parameters = method.getParameters();
        final var resolvedParameters = new Object[parameters.length];
        range(0, parameters.length)
                .forEach(index -> resolvedParameters[index] = resolveParameter(parameters[index], workflowRun));
        return resolvedParameters;
    }

    private Object resolveParameter(Parameter parameter, WorkflowRun workflowRun) {
        final var parameterIsMandatory = ofNullable(parameter.getDeclaredAnnotation(OptionalParam.class)).isEmpty();
        final var parameterType = parameter.getType();
        final var parameterizedType = parameter.getParameterizedType();
        if (parameterizedType instanceof ParameterizedType && List.class.equals(parameterType)) {
            final var actualTypeArguments = ((ParameterizedType) parameterizedType).getActualTypeArguments();
            return workflowRun.getCache()
                    .lookupList(parameter.getName(), (Class<?>) actualTypeArguments[0], parameterIsMandatory);
        } else if (WorkflowRun.class.equals(parameterType)) {
            return workflowRun;
        } else {
            return workflowRun.getCache().lookup(parameter.getName(), parameterType, parameterIsMandatory);
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

    private void runOutputs(Object output, WorkflowRunCache workflowRunCache) {
        outputs.forEach(out -> workflowRunCache.put(out.getName(), out.evaluate(Map.of(WORKFLOW_CACHE, workflowRunCache, OUTPUT, output))));
    }

}
