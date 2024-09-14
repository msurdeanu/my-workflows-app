package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.codehaus.janino.SimpleCompiler;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;
import org.myworkflows.domain.command.api.OptionalParam;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.List;
import java.util.Set;

import static java.lang.System.lineSeparator;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.joining;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class JavaCommand extends AbstractCommand {

    public JavaCommand(String name,
                       Set<ExpressionNameValue> ifs,
                       Set<ExpressionNameValue> inputs,
                       Set<ExpressionNameValue> asserts,
                       Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod
    public Object java(@MandatoryParam WorkflowRun workflowRun,
                       @MandatoryParam List<String> scriptLines,
                       @OptionalParam String className,
                       @OptionalParam String methodName) {
        if (scriptLines.isEmpty()) {
            return null;
        }

        final var resolvedClassName = ofNullable(className).orElse("DynamicClass");
        final var resolvedMethodName = ofNullable(methodName).orElse("run");
        final var resolvedScriptLines = scriptLines.stream().collect(joining(lineSeparator()));

        try {
            final var compiler = new SimpleCompiler();
            compiler.cook(resolvedScriptLines);

            final var dynamicClass = compiler.getClassLoader().loadClass(resolvedClassName);
            final var instance = dynamicClass.getConstructor().newInstance();

            final var runMethod = dynamicClass.getMethod(resolvedMethodName, WorkflowRunCache.class);
            return runMethod.invoke(instance, workflowRun.getCache());
        } catch (Exception e) {
            throw new WorkflowRuntimeException(e);
        }
    }

}
