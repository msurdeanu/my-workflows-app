package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.exception.WorkflowRuntimeException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SleepCommandTest {

    @Test
    public void whenMandatoryParameterIsSet_thenNoExceptionIsExpected() {
        // given
        final var executionContext = new ExecutionContext();
        executionContext.getCache().put("sleepTime", 100L);

        // when & then
        assertDoesNotThrow(() -> new SleepCommand().run(executionContext));
    }

    @Test
    public void whenMandatoryParameterIsNotSet_thenAnExceptionIsExpected() {
        // given
        final var executionContext = new ExecutionContext();
        executionContext.getCache().put("sleep", 100L);

        // when & then
        assertThrows(WorkflowRuntimeException.class, () -> new SleepCommand().run(executionContext));
    }

}
