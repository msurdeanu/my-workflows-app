package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.exception.WorkflowRuntimeException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SleepCommandTest {

    @Test
    public void whenMandatoryParameterIsSetThenNoExceptionIsExpected() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("sleepTime", 100L);

        // when & then
        assertDoesNotThrow(() -> new SleepCommand().run(workflowRun));
    }

    @Test
    public void whenMandatoryParameterIsNotSetThenAnExceptionIsExpected() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("sleep", 100L);

        // when & then
        assertThrows(WorkflowRuntimeException.class, () -> new SleepCommand().run(workflowRun));
    }

}
