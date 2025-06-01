package org.myworkflows.exception;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.2.0
 */
public final class WorkflowRuntimeExceptionTest {

    @Test
    public void testWrappingWhenNoExceptionIsRaised() {
        assertEquals(1, WorkflowRuntimeException.wrap(() -> 1));
    }

    @Test
    public void testWrappingWhenAnExceptionIsRaised() {
        final var builder = new StringBuilder();
        assertThrows(WorkflowRuntimeException.class, () -> WorkflowRuntimeException.wrap(() -> {
            throw new RuntimeException("test");
        }, exception -> builder.append(exception.getMessage())));
        assertEquals("test", builder.toString());
    }

}
