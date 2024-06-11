package org.myworkflows.domain.command;

import groovy.lang.GroovyShell;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;
import org.myworkflows.domain.command.api.OptionalParam;

import java.util.List;

import static java.lang.System.lineSeparator;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.joining;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class GroovyCommand extends AbstractCommand {

    private static final GroovyShell GROOVY_SHELL = new GroovyShell();

    @ExecutionMethod
    public Object groovy(@MandatoryParam WorkflowRun workflowRun,
                         @MandatoryParam List<String> scriptLines,
                         @OptionalParam String methodName) {
        final var resolvedMethodName = ofNullable(methodName).orElse("run");

        final var script = GROOVY_SHELL.parse(scriptLines.stream().collect(joining(lineSeparator())));
        return script.invokeMethod(resolvedMethodName, workflowRun);
    }

}
