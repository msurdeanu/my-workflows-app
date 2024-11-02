package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.commons.compiler.CompileException;
import org.codehaus.janino.SimpleCompiler;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.ParentClassLoaderHolder;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static java.lang.System.lineSeparator;
import static java.util.stream.Collectors.joining;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
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
    public Object java(@ExecutionParam WorkflowRun workflowRun,
                       @ExecutionParam List<String> scriptLines,
                       @ExecutionParam(required = false, defaultValue = "run") String methodName,
                       @ExecutionParam(required = false, defaultValue = "DynamicClass") String className,
                       @ExecutionParam(required = false) List<String> fileNames) {
        if (scriptLines.isEmpty()) {
            return null;
        }

        final var resolvedScriptLines = scriptLines.stream().collect(joining(lineSeparator()));
        try {
            final var compiler = new SimpleCompiler();
            compiler.setParentClassLoader(ParentClassLoaderHolder.INSTANCE.getClassLoader());
            processFileNames(compiler, fileNames);
            compiler.cook(resolvedScriptLines);

            final var dynamicClass = compiler.getClassLoader().loadClass(className);
            final var instance = dynamicClass.getConstructor().newInstance();

            final var runMethod = dynamicClass.getMethod(methodName, WorkflowRunCache.class);
            return runMethod.invoke(instance, workflowRun.getCache());
        } catch (Exception exception) {
            log.debug("Command '{}' thrown an exception.", getName(), exception);
            throw new WorkflowRuntimeException(exception);
        }
    }

    private void processFileNames(SimpleCompiler compiler, List<String> fileNames) throws CompileException, IOException {
        for (String fileName: fileNames) {
            if (!StringUtils.EMPTY.equals(fileName)) {
                compiler.cookFile(fileName);
            }
        }
    }

}
