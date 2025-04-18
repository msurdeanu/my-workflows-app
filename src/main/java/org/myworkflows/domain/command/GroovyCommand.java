package org.myworkflows.domain.command;

import groovy.lang.GroovyShell;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.holder.ParentClassLoaderHolder;

import java.util.List;
import java.util.Set;

import static java.lang.System.lineSeparator;
import static java.util.stream.Collectors.joining;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class GroovyCommand extends AbstractCommand {

    public static final String PREFIX = "groovy";

    public GroovyCommand(String name,
                         Set<ExpressionNameValue> ifs,
                         Set<ExpressionNameValue> inputs,
                         Set<ExpressionNameValue> asserts,
                         Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = PREFIX)
    public Object groovy(@ExecutionParam(bypassed = true) WorkflowRun workflowRun,
                         @ExecutionParam List<String> scriptLines,
                         @ExecutionParam(required = false, defaultValue = "run") String method) {
        if (scriptLines.isEmpty()) {
            return null;
        }

        final var script = new GroovyShell(ParentClassLoaderHolder.INSTANCE.getClassLoader())
            .parse(scriptLines.stream().collect(joining(lineSeparator())));
        return script.invokeMethod(method, workflowRun.getCache());
    }

}
