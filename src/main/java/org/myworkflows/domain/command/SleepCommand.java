package org.myworkflows.domain.command;

import static java.lang.Thread.sleep;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SleepCommand extends AbstractCommand {

    @ExecutionMethod
    public long sleepAWhile(@MandatoryParam final Number sleepTime) throws InterruptedException {
        final var startTime = System.currentTimeMillis();
        sleep(sleepTime.longValue());
        return System.currentTimeMillis() - startTime;
    }

}
