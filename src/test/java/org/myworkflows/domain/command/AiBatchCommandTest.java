package org.myworkflows.domain.command;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.AnthropicStub;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.apache.commons.lang3.exception.ExceptionUtils.getRootCause;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.myworkflows.domain.command.AiCommandTest.run;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class AiBatchCommandTest {

    private static final String SCRIPT = """
        commands:
          - name: tickets
            class: nothing
            inputs:
              - name: tickets
                value: [ "The database is down.", "A typo on the pricing page." ]
          - name: triage them all
            class: aiBatch
            inputs:
              - name: aiBatch.prompts
                class: groovy
                value: |
                  $(tickets).collect { "Triage this ticket: ${it}" }
              - name: aiBatch.schema
                value:
                  type: object
                  properties:
                    severity: { enum: [low, high, outage] }
            outputs:
              - name: triages
                class: groovy
                value: output
        """;

    private AnthropicStub anthropic;

    @BeforeEach
    public void setUp() {
        anthropic = AnthropicStub.connect();
    }

    @AfterEach
    public void tearDown() {
        anthropic.close();
    }

    @Test
    public void whenEveryPromptIsAnsweredThenTheAnswersComeInTheOrderOfThePrompts() {
        // given
        anthropic.respond(200, AnthropicStub.batch("ended"))
            .respondLines(
                AnthropicStub.succeeded(1, AnthropicStub.message("{\"severity\":\"low\"}", "end_turn")),
                AnthropicStub.succeeded(0, AnthropicStub.message("{\"severity\":\"outage\"}", "end_turn")));
        final var workflowRun = new WorkflowRun();

        // when
        run(SCRIPT, workflowRun);

        // then
        assertEquals(List.of(Map.of("severity", "outage"), Map.of("severity", "low")), workflowRun.getCache().get("triages"));
        final var body = anthropic.requests().getFirst().body();
        assertTrue(body.contains("Triage this ticket: The database is down."));
        assertTrue(body.contains("Triage this ticket: A typo on the pricing page."));
    }

    @Test
    public void whenAPromptIsNotAnsweredThenTheCommandFailsAndSaysWhich() {
        // given
        anthropic.respond(200, AnthropicStub.batch("ended"))
            .respondLines(
                AnthropicStub.succeeded(0, AnthropicStub.message("{\"severity\":\"outage\"}", "end_turn")),
                AnthropicStub.errored(1, "Overloaded"));
        final var workflowRun = new WorkflowRun();

        // when
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> run(SCRIPT, workflowRun));

        // then
        assertEquals("1 of 2 prompts got no answer - prompt 2: The call to Claude failed: Overloaded", getRootCause(exception).getMessage());
    }

    @Test
    public void whenFailuresAreAllowedThenAPromptWhichIsNotAnsweredHasNoAnswer() {
        // given
        anthropic.respond(200, AnthropicStub.batch("ended"))
            .respondLines(
                AnthropicStub.succeeded(0, AnthropicStub.message("{\"severity\":\"outage\"}", "end_turn")),
                AnthropicStub.succeeded(1, AnthropicStub.message("{\"severity\":", "max_tokens")));
        final var workflowRun = new WorkflowRun();

        // when
        run(SCRIPT.replace("      - name: aiBatch.schema\n",
            "      - name: aiBatch.failOnError\n        value: false\n      - name: aiBatch.schema\n"), workflowRun);

        // then
        assertEquals(Arrays.asList(Map.of("severity", "outage"), null), workflowRun.getCache().get("triages"));
    }

}
