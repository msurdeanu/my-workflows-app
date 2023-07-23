package org.myworkflows.domain.command;

import org.myworkflows.domain.ExecutionContext;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class PrintCommand extends AbstractCommand {

    @ExecutionMethod
    public int print(@MandatoryParam final ExecutionContext executionContext,
                     @MandatoryParam final List<String> keys) {
        return keys.stream()
            .map(executionContext::markKeyAsPrinted)
            .mapToInt(isPrinted -> isPrinted ? 1 : 0)
            .sum();
    }

}
