package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ExecutionContext;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WaitUntilSubPassesCommandTest {

    @Test
    public void whenMandatoryParameterIsSet_thenNoExceptionIsExpected() {
        // given
        final var executionContext = new ExecutionContext();

        // when & then
        assertDoesNotThrow(() -> new WaitUntilSubPassesCommand().run(executionContext));
    }

}
