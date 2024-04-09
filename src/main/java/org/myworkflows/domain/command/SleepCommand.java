package org.myworkflows.domain.command;

import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SleepCommand extends AbstractCommand {

    @ExecutionMethod
    public long sleep(@MandatoryParam final Number sleepTime) throws InterruptedException {
        final var startTime = System.currentTimeMillis();
        Thread.sleep(sleepTime.longValue());
        return System.currentTimeMillis() - startTime;
    }

}
