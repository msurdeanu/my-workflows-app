package org.myworkflows.holder;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ai.AiCall;
import org.myworkflows.exception.AiException;
import org.myworkflows.exception.AiException.Reason;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class AiClientHolderBatchTest {

    private static final Duration POLL_INTERVAL = Duration.ofMillis(20);

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
    public void whenBatchIsSentThenEveryCallIsARequestAndTheResultsComeInTheOrderOfTheCalls() {
        // given
        anthropic.respond(200, AnthropicStub.batch("in_progress"))
            .respond(200, AnthropicStub.batch("ended"))
            .respondLines(
                AnthropicStub.errored(1, "prompt is too long"),
                AnthropicStub.succeeded(2, """
                    {"id":"msg_3","type":"message","role":"assistant","model":"claude-opus-5-5","content":[],
                     "stop_reason":"refusal","stop_details":{"type":"refusal","category":"cyber","explanation":"Not this one."},
                     "stop_sequence":null,"usage":{"input_tokens":5,"output_tokens":0}}""".replace("\n", "")),
                AnthropicStub.succeeded(0, AnthropicStub.message("{\"severity\":\"low\"}", "end_turn")));
        final var schema = Map.of("type", "object", "properties", Map.of("severity", Map.of("enum", List.of("low", "high"))));

        // when
        final var results = AiClientHolder.INSTANCE.sendBatch(List.of(
            AiCall.builder().prompt("Ticket 1 from hunter22").system("Triage.").schema(schema).build(),
            AiCall.builder().prompt("Ticket 2").schema(schema).build(),
            AiCall.builder().prompt("Ticket 3").model("claude-haiku-5-5").schema(schema).build()), null, POLL_INTERVAL);

        // then the results are in the order of the calls
        assertEquals(3, results.size());
        assertTrue(results.get(0).isAnswered());
        assertEquals("{\"severity\":\"low\"}", results.get(0).answer().text());
        assertFalse(results.get(1).isAnswered());
        assertEquals("The call to Claude failed: prompt is too long", results.get(1).failure());
        assertNull(results.get(2).answer());
        assertEquals("Claude declined to answer. Category: cyber. Not this one.", results.get(2).failure());

        // and the batch was created, followed until it ended, and read
        final var requests = anthropic.requests();
        assertEquals(List.of("POST /v1/messages/batches", "GET /v1/messages/batches/msgbatch_1", "GET /v1/messages/batches/msgbatch_1/results"),
            requests.stream().map(request -> request.method() + " " + request.path()).toList());

        // with one request per call, masked, held to the schema and without the fallbacks, which a batch does not take
        final var body = requests.getFirst().body();
        assertTrue(body.contains("\"custom_id\":\"item-0\""));
        assertTrue(body.contains("\"custom_id\":\"item-2\""));
        assertTrue(body.contains("Ticket 1 from ********"));
        assertFalse(body.contains(AnthropicStub.SECRET));
        assertTrue(body.contains("\"system\":\"Triage.\""));
        assertTrue(body.contains("\"model\":\"claude-haiku-5-5\""));
        assertTrue(body.contains("\"type\":\"json_schema\""));
        assertTrue(body.contains("\"additionalProperties\":false"));
        assertFalse(body.contains("fallbacks"));
    }

    @Test
    public void whenBatchOutlastsItsTimeoutThenItIsCancelled() {
        // given
        anthropic.respond(200, AnthropicStub.batch("in_progress"))
            .respondByDefault(AnthropicStub.batch("in_progress"));

        // when
        final var exception = assertThrows(AiException.class, () -> AiClientHolder.INSTANCE.sendBatch(
            List.of(AiCall.builder().prompt("hi").build()), Duration.ofMillis(300), POLL_INTERVAL));

        // then
        assertEquals(Reason.TIMEOUT, exception.getReason());
        anthropic.awaitRequest(request -> "/v1/messages/batches/msgbatch_1/cancel".equals(request.path()));
    }

    @Test
    public void whenWaitingThreadIsInterruptedThenTheBatchIsCancelled() throws InterruptedException {
        // given
        anthropic.respond(200, AnthropicStub.batch("in_progress"))
            .respondByDefault(AnthropicStub.batch("in_progress"));
        final var failure = new AtomicReference<AiException>();
        final var thread = Thread.ofPlatform().start(() -> {
            try {
                AiClientHolder.INSTANCE.sendBatch(List.of(AiCall.builder().prompt("hi").build()), null, POLL_INTERVAL);
            } catch (AiException exception) {
                failure.set(exception);
            }
        });

        // when a run is cancelled while it waits for the batch
        anthropic.awaitRequest(request -> "GET".equals(request.method()));
        thread.interrupt();
        thread.join(Duration.ofSeconds(5));

        // then
        assertEquals(Reason.CANCELLED, failure.get().getReason());
        anthropic.awaitRequest(request -> "/v1/messages/batches/msgbatch_1/cancel".equals(request.path()));
    }

    @Test
    public void whenThereIsNothingToAskThenNothingIsSent() {
        assertEquals(List.of(), AiClientHolder.INSTANCE.sendBatch(List.of(), null));
        assertTrue(anthropic.requests().isEmpty());
    }

    @Test
    public void whenSchemaSaysNothingAboutOtherFieldsThenEveryObjectOfItRefusesThem() {
        // given
        final Map<String, Object> schema = Map.of(
            "type", "object",
            "properties", Map.of(
                "type", Map.of("type", "string"),
                "owner", Map.of("type", "object", "properties", Map.of("name", Map.of("type", "string"))),
                "tags", Map.of("type", "array", "items", Map.of("properties", Map.of("label", Map.of("type", "string")))),
                "extra", Map.of("type", "object", "additionalProperties", true)),
            "$defs", Map.of("address", Map.of("type", List.of("object", "null"))),
            "anyOf", List.of(Map.of("type", "object"), Map.of("type", "string")));

        // when
        final var closed = AiClientHolder.closeObjects(schema);

        // then
        assertEquals(false, closed.get("additionalProperties"));
        final var properties = (Map<?, ?>) closed.get("properties");
        assertEquals(Map.of("type", "string"), properties.get("type"));
        assertEquals(false, ((Map<?, ?>) properties.get("owner")).get("additionalProperties"));
        assertEquals(false, ((Map<?, ?>) ((Map<?, ?>) properties.get("tags")).get("items")).get("additionalProperties"));
        assertEquals(true, ((Map<?, ?>) properties.get("extra")).get("additionalProperties"));
        assertEquals(false, ((Map<?, ?>) ((Map<?, ?>) closed.get("$defs")).get("address")).get("additionalProperties"));
        assertEquals(false, ((Map<?, ?>) ((List<?>) closed.get("anyOf")).get(0)).get("additionalProperties"));
        assertFalse(((Map<?, ?>) ((List<?>) closed.get("anyOf")).get(1)).containsKey("additionalProperties"));
    }

}
