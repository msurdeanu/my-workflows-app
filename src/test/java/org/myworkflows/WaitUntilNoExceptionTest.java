package org.myworkflows;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.command.WaitUntilSubPassesCommand;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WaitUntilNoExceptionTest {

    @Test
    public void whenMandatoryParameterIsSet_thenNoExceptionIsExpected() {
        // given
        final var executionContext = new ExecutionContext();

        // when & then
        assertDoesNotThrow(() -> new WaitUntilSubPassesCommand().run(executionContext));
    }

}
