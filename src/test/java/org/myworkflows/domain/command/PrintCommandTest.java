package org.myworkflows.domain.command;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunPrint;

import java.util.List;

import static java.util.stream.IntStream.range;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class PrintCommandTest {

    @Test
    public void whenPrintCommandIsCalledThenShouldWorkCorrectly() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("prop1", 1);
        workflowRun.getCache().put("prop2", 2);

        // when & then
        assertEquals(1, new PrintCommand().print(workflowRun, List.of("prop1", "prop3")));
        final var allPrints = workflowRun.getAllPrints();
        assertEquals(1, allPrints.size());
        final var firstPrint = allPrints.getFirst();
        assertInstanceOf(WorkflowRunPrint.class, firstPrint);
        assertEquals("prop1", firstPrint.name());
        assertEquals(1, firstPrint.value());
        assertEquals("integer", firstPrint.type());
    }

    @Test
    public void whenPropertyValueIsTooLongThenAbbreviationHappens() {
        // given
        final var workflowRun = new WorkflowRun();
        workflowRun.getCache().put("prop", createLongValueString());

        // when & then
        assertEquals(1, new PrintCommand().print(workflowRun, List.of("prop")));
        final var allPrints = workflowRun.getAllPrints();
        assertEquals(1, allPrints.size());
        final var firstPrint = allPrints.getFirst();
        assertInstanceOf(WorkflowRunPrint.class, firstPrint);
        assertEquals("prop", firstPrint.name());
        assertEquals(64, firstPrint.abbrValue().length());
        assertEquals("string", firstPrint.type());
    }

    private String createLongValueString() {
        final var builder = new StringBuilder();
        range(0, 100).forEach(item -> builder.append("Value is too long."));
        return builder.toString();
    }

}
