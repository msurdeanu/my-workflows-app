package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;

import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class SleepCommand extends AbstractCommand {

    public SleepCommand(String name,
                        Set<ExpressionNameValue> ifs,
                        Set<ExpressionNameValue> inputs,
                        Set<ExpressionNameValue> asserts,
                        Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod
    public long sleep(@MandatoryParam Number sleepTime) throws InterruptedException {
        final var startTime = System.currentTimeMillis();
        Thread.sleep(sleepTime.longValue());
        return System.currentTimeMillis() - startTime;
    }

}
