package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;

import java.util.List;
import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class PrintCommand extends AbstractCommand {

    public PrintCommand(String name,
                        Set<ExpressionNameValue> ifs,
                        Set<ExpressionNameValue> inputs,
                        Set<ExpressionNameValue> asserts,
                        Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod
    public int print(@MandatoryParam WorkflowRun workflowRun,
                     @MandatoryParam List<String> keys) {
        return keys.stream()
            .map(workflowRun::markKeyAsPrinted)
            .mapToInt(isPrinted -> isPrinted ? 1 : 0)
            .sum();
    }

}
