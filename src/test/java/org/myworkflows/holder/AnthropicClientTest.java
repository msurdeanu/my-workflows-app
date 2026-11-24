package org.myworkflows.holder;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.holder.AnthropicClient.ApiException;

import java.time.Duration;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.6
 */
public final class AnthropicClientTest {

    private static final Duration WAIT = Duration.ofSeconds(10);

    private AnthropicStub anthropic;
    private AnthropicClient client;

    @BeforeEach
    public void setUp() {
        anthropic = AnthropicStub.connect();
        client = new AnthropicClient("sk-ant-test", anthropic.baseUrl(), 2);
    }

    @AfterEach
    public void tearDown() {
        client.close();
        anthropic.close();
    }

    @Test
    public void whenAnthropicIsOverloadedThenTheRequestIsSentAgain() throws Exception {
        // given
        anthropic.respond(529, "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Overloaded\"}}")
            .respond(200, AnthropicStub.message("Hello", "end_turn"));

        // when
        final var answer = client.post("/v1/messages", Map.of("model", "claude-opus-5-5"), Map.of(), WAIT);

        // then
        assertEquals("msg_1", answer.path("id").stringValue());
        assertEquals(2, anthropic.requests().size());
        assertEquals(anthropic.requests().get(0).body(), anthropic.requests().get(1).body());
    }

    @Test
    public void whenRequestIsWrongThenItIsNotSentAgain() {
        // given
        anthropic.respond(400, "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"max_tokens: too many\"}}");

        // when
        final var exception = assertThrows(ApiException.class, () -> client.post("/v1/messages", Map.of(), Map.of(), WAIT));

        // then
        assertEquals(400, exception.getStatus());
        assertEquals("400: max_tokens: too many", exception.getMessage());
        assertEquals(1, anthropic.requests().size());
    }

    @Test
    public void whenEveryAttemptFailsThenTheLastFailureIsTold() {
        // given
        final var failure = "{\"type\":\"error\",\"error\":{\"type\":\"api_error\",\"message\":\"Internal server error\"}}";
        anthropic.respond(500, failure).respond(500, failure).respond(500, failure).respond(200, "{}");

        // when
        final var exception = assertThrows(ApiException.class, () -> client.get("/v1/models/claude-opus-5-5", WAIT));

        // then the first attempt and the two retries were sent, no more
        assertEquals(500, exception.getStatus());
        assertEquals(3, anthropic.requests().size());
    }

    @Test
    public void whenAnswerIsALinePerObjectThenEveryLineIsRead() throws Exception {
        // given
        anthropic.respondLines(AnthropicStub.succeeded(0, AnthropicStub.message("a", "end_turn")), AnthropicStub.errored(1, "no"));

        // when
        final var lines = client.getLines("/v1/messages/batches/msgbatch_1/results", WAIT);

        // then
        assertEquals(2, lines.size());
        assertEquals("item-1", lines.get(1).path("custom_id").stringValue());
    }

    @Test
    public void whenNothingSaysHowLongToWaitThenTheBackoffGrowsUpToItsLimit() {
        final var first = AnthropicClient.backoff(1, null);
        assertTrue(first.toMillis() >= 375 && first.toMillis() <= 500, first.toString());
        final var late = AnthropicClient.backoff(20, null);
        assertTrue(late.toMillis() >= 6_000 && late.toMillis() <= 8_000, late.toString());
    }

    @Test
    public void whenValueIsAPathSegmentThenItIsEncoded() {
        assertEquals("claude-opus-5-5", AnthropicClient.pathSegment("claude-opus-5-5"));
        assertEquals("a%2Fb%20c", AnthropicClient.pathSegment("a/b c"));
    }

}
