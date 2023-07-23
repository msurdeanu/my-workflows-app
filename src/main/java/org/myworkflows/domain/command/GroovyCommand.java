package org.myworkflows.domain.command;

import groovy.lang.GroovyShell;
import org.myworkflows.domain.ExecutionContext;

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
    public Object runScript(@MandatoryParam final ExecutionContext executionContext,
                            @MandatoryParam final List<String> scriptLines,
                            @OptionalParam final String methodName) {
        final var resolvedMethodName = ofNullable(methodName).orElse("run");

        final var script = GROOVY_SHELL.parse(scriptLines.stream().collect(joining(lineSeparator())));
        return script.invokeMethod(resolvedMethodName, executionContext);
    }

}
