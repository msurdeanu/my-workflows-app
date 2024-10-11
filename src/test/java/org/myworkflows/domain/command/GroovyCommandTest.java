package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class GroovyCommandTest {

    @Test
    public void whenSimpleGroovyCodeIsRunThenEverythingWorksAsExpected() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("scriptLines", List.of(
            "import org.myworkflows.domain.WorkflowRunCache",
            "def String run(WorkflowRunCache cache) {",
            "  return \"Test\"",
            "}"
        ));

        // when & then
        assertDoesNotThrow(() -> new GroovyCommand("A", Set.of(), Set.of(), Set.of(),
            Set.of(new ExpressionNameValue("var", "#output", RuntimeEvaluator.SPEL))).run(workflowRun));
        assertEquals("Test", workflowRun.getCache().get("var", String.class));
    }

    @Test
    public void whenSimpleGroovyCodeIsRunWithOptionalParamsThenEverythingWorksAsExpected() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("scriptLines", List.of(
            "import org.myworkflows.domain.WorkflowRunCache",
            "def int myRun(WorkflowRunCache cache) {",
            "  return 1",
            "}"
        ));
        workflowRun.getCache().put("methodName", "myRun");

        // when & then
        assertDoesNotThrow(() -> new GroovyCommand("A", Set.of(), Set.of(), Set.of(),
            Set.of(new ExpressionNameValue("var", "#output", RuntimeEvaluator.SPEL))).run(workflowRun));
        assertEquals(1, workflowRun.getCache().get("var", Integer.class));
    }

    @Test
    public void whenSimpleGroovyCodeIsRunWithSyntaxIssueThenAProperExceptionIsRaised() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("scriptLines", List.of(
            "import org.myworkflows.domain.WorkflowRunCache",
            "def int myRun(WorkflowRunCache cache) {",
            "  return 1",
            "" // we forgot "}"
        ));
        workflowRun.getCache().put("methodName", "myRun");

        // when & then
        assertThrows(WorkflowRuntimeException.class,
            () -> new JavaCommand("A", Set.of(), Set.of(), Set.of(), Set.of()).run(workflowRun));
    }

}
