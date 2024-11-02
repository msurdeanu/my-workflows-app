package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.RuntimeEvaluator;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class HttpRequestCommandTest {

    @Test
    public void whenGetRequestIsMadeThenEverythingWorksAsExpected() {
        // given
        final var workflowRun = new WorkflowRun();

        // when & then
        assertDoesNotThrow(() -> new HttpRequestCommand("T",
            Set.of(),
            Set.of(new ExpressionNameValue("httpRequest.url", "https://httpbin.dev/json", RuntimeEvaluator.PLAIN)),
            Set.of(new ExpressionNameValue("assert", "output.getStatusCode().value() == 200", RuntimeEvaluator.JAVA)),
            Set.of(new ExpressionNameValue("body", "output.getBody()", RuntimeEvaluator.JAVA)))
            .run(workflowRun));
        assertTrue(workflowRun.getCache().get("body", String.class).contains("Yours Truly"));
    }

    @Test
    public void whenPostRequestIsMadeThenExceptionIsRaised() {
        // given
        final var workflowRun = new WorkflowRun();

        // when & then
        assertThrows(WorkflowRuntimeException.class, () -> new HttpRequestCommand("T", Set.of(),
            Set.of(new ExpressionNameValue("httpRequest.url", "https://httpbin.dev/get", RuntimeEvaluator.PLAIN),
                new ExpressionNameValue("httpRequest.method", "POST", RuntimeEvaluator.PLAIN)),
            Set.of(new ExpressionNameValue("assert", "output.getStatusCode().value() == 200", RuntimeEvaluator.JAVA)),
            Set.of(new ExpressionNameValue("body", "output.getBody()", RuntimeEvaluator.JAVA)))
            .run(workflowRun));
    }

}
