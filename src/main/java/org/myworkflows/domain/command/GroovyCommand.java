package org.myworkflows.domain.command;

import groovy.lang.GroovyShell;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;

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

    private static final GroovyShell GROOVY_SHELL = new GroovyShell();

    public GroovyCommand(String name,
                         Set<ExpressionNameValue> ifs,
                         Set<ExpressionNameValue> inputs,
                         Set<ExpressionNameValue> asserts,
                         Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = "groovy")
    public Object groovy(@ExecutionParam WorkflowRun workflowRun,
                         @ExecutionParam List<String> scriptLines,
                         @ExecutionParam(required = false, defaultValue = "run") String methodName) {
        if (scriptLines.isEmpty()) {
            return null;
        }

        final var script = GROOVY_SHELL.parse(scriptLines.stream().collect(joining(lineSeparator())));
        return script.invokeMethod(methodName, workflowRun.getCache());
    }

}
