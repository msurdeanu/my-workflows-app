package org.myworkflows.domain.command;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.exception.AiException;
import org.myworkflows.exception.AiException.Reason;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.AnthropicStub;

import java.util.List;
import java.util.Map;

import static org.apache.commons.lang3.exception.ExceptionUtils.getRootCause;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class AiCommandTest {

    private static final String TRIAGE = """
        commands:
          - name: ticket
            class: nothing
            inputs:
              - name: ticketBody
                value: The database is down for every customer.
          - name: triage
            class: ai
            inputs:
              - name: ai.prompt
                class: groovy
                value: "'Triage this ticket: ' + $(ticketBody)"
              - name: ai.schema
                value:
                  type: object
                  properties:
                    severity: { enum: [low, high, outage] }
                    summary: { type: string }
                  required: [severity, summary]
            outputs:
              - name: triage
                class: groovy
                value: output
          - name: page somebody
            class: nothing
            ifs:
              - name: is an outage
                class: groovy
                value: $(triage).severity == 'outage'
            inputs:
              - name: paged
                value: done
          - name: file it
            class: nothing
            ifs:
              - name: is low
                class: groovy
                value: $(triage).severity == 'low'
            inputs:
              - name: filed
                value: done
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
    public void whenSchemaIsGivenThenTheAnswerIsAMapWhichTheCommandsAfterItBranchOn() {
        // given
        anthropic.respond(200, AnthropicStub.message("{\"severity\":\"outage\",\"summary\":\"The database is down.\"}", "end_turn"));
        final var workflowRun = new WorkflowRun();

        // when
        run(TRIAGE, workflowRun);

        // then the answer is a map, in the order of its fields
        final var triage = workflowRun.getCache().get("triage");
        assertInstanceOf(Map.class, triage);
        assertEquals(List.of("severity", "summary"), List.copyOf(((Map<?, ?>) triage).keySet()));
        assertEquals("outage", ((Map<?, ?>) triage).get("severity"));
        assertEquals("done", workflowRun.getCache().get("paged"));
        assertNull(workflowRun.getCache().get("filed"));

        // and Claude was held to the schema, which says that an object takes no other field
        final var body = anthropic.requests().getFirst().body();
        assertTrue(body.contains("Triage this ticket: The database is down for every customer."));
        assertTrue(body.contains("\"format\":{\"schema\":{\"type\":\"object\""));
        assertTrue(body.contains("\"enum\":[\"low\",\"high\",\"outage\"]"));
        assertTrue(body.contains("\"additionalProperties\":false"));
        assertTrue(body.contains("\"type\":\"json_schema\""));
    }

    @Test
    public void whenNoSchemaIsGivenThenTheAnswerIsItsTextAndTheInputsAreSent() {
        // given
        anthropic.respond(200, AnthropicStub.message("Two disks are almost full.", "end_turn"));
        final var workflowRun = new WorkflowRun();

        // when
        run("""
            commands:
              - name: sum it up
                class: ai
                inputs:
                  - name: ai.prompt
                    value: "Sum up: /dev/sda1 98%, /dev/sdb1 97%"
                  - name: ai.system
                    value: You are terse.
                  - name: ai.model
                    value: claude-haiku-5-5
                  - name: ai.maxTokens
                    value: 512
                  - name: ai.effort
                    value: LOW
                outputs:
                  - name: summary
                    class: groovy
                    value: output
            """, workflowRun);

        // then
        assertEquals("Two disks are almost full.", workflowRun.getCache().get("summary"));
        final var body = anthropic.requests().getFirst().body();
        assertTrue(body.contains("\"model\":\"claude-haiku-5-5\""));
        assertTrue(body.contains("\"max_tokens\":512"));
        assertTrue(body.contains("\"effort\":\"low\""));
        assertTrue(body.contains("\"system\":\"You are terse.\""));
        assertFalse(body.contains("json_schema"));
    }

    @Test
    public void whenAnEarlierCommandLeftASchemaThenAnEmptyOneAsksForATextAgain() {
        // given
        anthropic.respond(200, AnthropicStub.message("{\"severity\":\"low\",\"summary\":\"Nothing much.\"}", "end_turn"))
            .respond(200, AnthropicStub.message("Dear customer, ...", "end_turn"));
        final var workflowRun = new WorkflowRun();
        run(TRIAGE, workflowRun);

        // when the next command sets the schema to an empty map
        run("""
            commands:
              - name: write back
                class: ai
                inputs:
                  - name: ai.prompt
                    value: Write to the customer.
                  - name: ai.schema
                    value: {}
                outputs:
                  - name: reply
                    class: groovy
                    value: output
            """, workflowRun);

        // then
        assertEquals("Dear customer, ...", workflowRun.getCache().get("reply"));
        assertFalse(anthropic.requests().get(1).body().contains("json_schema"));
    }

    @Test
    public void whenAnswerIsCutAtTheLimitThenTheCommandFails() {
        // given
        anthropic.respond(200, AnthropicStub.message("{\"severity\":\"hi", "max_tokens"));
        final var workflowRun = new WorkflowRun();

        // when
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> run(TRIAGE, workflowRun));

        // then
        final var cause = assertInstanceOf(AiException.class, getRootCause(exception));
        assertEquals(Reason.TRUNCATED, cause.getReason());
        assertNull(workflowRun.getCache().get("triage"));
    }

    @Test
    public void whenClaudeIsTurnedOffThenTheCommandFailsWithoutSendingAnything() {
        // given
        anthropic.close();
        final var workflowRun = new WorkflowRun();

        // when
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> run(TRIAGE, workflowRun));

        // then
        assertEquals(Reason.DISABLED, assertInstanceOf(AiException.class, getRootCause(exception)).getReason());
        assertTrue(anthropic.requests().isEmpty());
    }

    static void run(String script, WorkflowRun workflowRun) {
        toObject(script, WorkflowDefinitionScript.class).getCommands().forEach(command -> command.run(workflowRun));
    }

}
