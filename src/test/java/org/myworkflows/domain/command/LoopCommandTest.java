package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class LoopCommandTest {

    @Test
    public void whenLoopCommandIsRunThenSubCommandIsRunSequentially() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("loop.items", List.of("a", "b", "c"));
        workflowRun.getCache().put("loop.backoffPeriod", 0);
        workflowRun.getCache().put("print.keys", List.of("loop.item"));
        final var printSubCommand = new PrintCommand("P", Set.of(), Set.of(), Set.of(), Set.of());

        // when & then
        new LoopCommand("A", Set.of(), Set.of(), Set.of(), Set.of(), List.of(printSubCommand)).run(workflowRun);
        assertEquals("c", workflowRun.getCache().get("loop.item"));
    }

    @Test
    public void whenLoopCommandIsRunAndBackoffPeriodIsSetThenEverythingWorksCorrectly() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("loop.items", List.of("a", "b", "c"));
        workflowRun.getCache().put("loop.backoffPeriod", 2000);
        workflowRun.getCache().put("print.keys", List.of("loop.item"));
        final var printSubCommand = new PrintCommand("P", Set.of(), Set.of(), Set.of(), Set.of());

        // when & then
        final var start = System.currentTimeMillis();
        new LoopCommand("A", Set.of(), Set.of(), Set.of(), Set.of(), List.of(printSubCommand)).run(workflowRun);
        final var duration = System.currentTimeMillis() - start;
        assertEquals("c", workflowRun.getCache().get("loop.item"));
        assertTrue(duration >= 4000);
    }

}
