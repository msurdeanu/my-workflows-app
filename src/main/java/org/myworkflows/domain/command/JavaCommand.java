package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.codehaus.janino.SimpleCompiler;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.ParentClassLoaderHolder;

import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@NoArgsConstructor
public final class JavaCommand extends AbstractCommand {

    public static final String PREFIX = "java";

    public JavaCommand(String name,
                       Set<ExpressionNameValue> ifs,
                       Set<ExpressionNameValue> inputs,
                       Set<ExpressionNameValue> asserts,
                       Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = PREFIX)
    public Object java(@ExecutionParam(bypassed = true) WorkflowRun workflowRun,
                       @ExecutionParam String script,
                       @ExecutionParam(required = false, defaultValue = "run") String method,
                       @ExecutionParam(required = false, defaultValue = "DynamicClass") String clazz,
                       @ExecutionParam(required = false, defaultValue = "11") Number sourceVersion,
                       @ExecutionParam(required = false, defaultValue = "11") Number targetVersion) {
        if (script.isEmpty()) {
            return null;
        }

        return WorkflowRuntimeException.wrap(() -> {
            final var compiler = new SimpleCompiler();
            compiler.setParentClassLoader(ParentClassLoaderHolder.INSTANCE.getClassLoader());
            compiler.setSourceVersion(sourceVersion.intValue());
            compiler.setTargetVersion(targetVersion.intValue());
            compiler.cook(script);

            final var dynamicClass = compiler.getClassLoader().loadClass(clazz);
            final var instance = dynamicClass.getConstructor().newInstance();

            final var runMethod = dynamicClass.getMethod(method, WorkflowRunCache.class);
            return runMethod.invoke(instance, workflowRun.getCache());
        }, exception -> log.debug("Command '{}' thrown an exception.", getName(), exception));
    }

}
