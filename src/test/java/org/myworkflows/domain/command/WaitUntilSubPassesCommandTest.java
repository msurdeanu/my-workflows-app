package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WaitUntilSubPassesCommandTest {

    @Test
    public void whenMandatoryParameterIsSetThenNoExceptionIsExpected() {
        // given
        final var workflowRun = new WorkflowRun();

        // when & then
        assertDoesNotThrow(() -> new WaitUntilSubPassesCommand().run(workflowRun));
    }

}
