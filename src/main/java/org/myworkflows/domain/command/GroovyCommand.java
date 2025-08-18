package org.myworkflows.domain.command;

import groovy.lang.GroovyShell;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.holder.ParentClassLoaderHolder;

import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0
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
                         @ExecutionParam String script,
                         @ExecutionParam(required = false, defaultValue = "run") String method) {
        if (script.isEmpty()) {
            return null;
        }

        return new GroovyShell(ParentClassLoaderHolder.INSTANCE.getClassLoader())
            .parse(script)
            .invokeMethod(method, workflowRun.getCache());
    }

}
