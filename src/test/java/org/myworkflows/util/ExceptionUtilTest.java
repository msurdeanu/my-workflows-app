package org.myworkflows.util;

import org.junit.jupiter.api.Test;
import org.myworkflows.exception.WorkflowRuntimeException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.myworkflows.util.ExceptionUtil.getMessageAndCause;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ExceptionUtilTest {

    @Test
    public void testExceptionWithoutCause() {
        // when
        final var messageAndCause = getMessageAndCause(new WorkflowRuntimeException("My test exception"));
        // then
        assertEquals("My test exception", messageAndCause);
    }

    @Test
    public void testExceptionWithCause() {
        // when
        final var messageAndCause = getMessageAndCause(new WorkflowRuntimeException(new WorkflowRuntimeException("My root cause")));
        // then
        assertTrue(messageAndCause.contains("Cause type: WorkflowRuntimeException"));
        assertTrue(messageAndCause.contains("Cause message: My root cause"));
        assertTrue(messageAndCause.contains("Cause stacktrace:"));
    }

}
