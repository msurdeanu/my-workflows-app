package org.myworkflows.domain.command;

import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class PrintCommand extends AbstractCommand {

    @ExecutionMethod
    public int print(@MandatoryParam WorkflowRun workflowRun,
                     @MandatoryParam List<String> keys) {
        return keys.stream()
            .map(workflowRun::markKeyAsPrinted)
            .mapToInt(isPrinted -> isPrinted ? 1 : 0)
            .sum();
    }

}
