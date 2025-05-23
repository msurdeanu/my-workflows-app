package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SleepCommandTest {

    @Test
    public void whenMandatoryParameterIsSetAndAssertIsProvidedThenNoExceptionIsExpected() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("sleep.time", 100L);

        // when and then
        assertDoesNotThrow(() -> new SleepCommand("T", Set.of(), Set.of(),
            Set.of(new ExpressionNameValue("assert", "output >= 100L", RuntimeEvaluator.JAVA)), Set.of()).run(workflowRun));
    }

    @Test
    public void whenMandatoryParameterIsNotSetThenAnExceptionIsExpected() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("sleep", 100L);

        // when and then
        assertThrows(WorkflowRuntimeException.class, () -> new SleepCommand().run(workflowRun));
    }

    @Test
    public void whenIfConditionIsNotMetThenNoExceptionIsExpected() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("sleep", 100L);

        // when and then
        assertDoesNotThrow(() -> new SleepCommand("T", Set.of(new ExpressionNameValue("var", "1 == 0", RuntimeEvaluator.JAVA)),
            Set.of(), Set.of(), Set.of()).run(workflowRun));
    }

}
