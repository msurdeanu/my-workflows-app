package org.myworkflows.holder;

import com.anthropic.client.okhttp.AnthropicOkHttpClient;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.ai.AiCall;
import org.myworkflows.domain.ai.AiStatus;
import org.myworkflows.exception.AiException;
import org.myworkflows.exception.AiException.Reason;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Talks to a server of its own, which plays Anthropic: what is sent is read as it arrives, and nothing leaves the test.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class AiClientHolderTest {

    private static final String TEXT_ANSWER = """
        {"id":"msg_1","type":"message","role":"assistant","model":"claude-opus-5-5",
         "content":[{"type":"text","text":"Hello "},{"type":"text","text":"there"}],
         "stop_reason":"end_turn","stop_sequence":null,"usage":{"input_tokens":12,"output_tokens":3}}""";

    private final List<Request> requests = new CopyOnWriteArrayList<>();
    private final Queue<Response> responses = new ArrayDeque<>();
    private final AtomicBoolean enabled = new AtomicBoolean(true);
    private final AtomicReference<String> model = new AtomicReference<>("claude-opus-5-5");
    private final AtomicInteger clientsBuilt = new AtomicInteger();

    private HttpServer server;

    @BeforeEach
    public void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
        server.setExecutor(Executors.newCachedThreadPool());
        server.createContext("/", this::handle);
        server.start();
        configure(true);
    }

    @AfterEach
    public void tearDown() {
        AiClientHolder.INSTANCE.configure(() -> false, () -> AiClientHolder.DEFAULT_MODEL, List::of, Optional::empty);
        server.stop(0);
    }

    @Test
    public void whenClaudeIsTurnedOffThenNothingIsSent() {
        // given
        enabled.set(false);

        // when
        final var exception = assertThrows(AiException.class, () -> AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").build()));

        // then
        assertEquals(Reason.DISABLED, exception.getReason());
        assertEquals(AiStatus.DISABLED, AiClientHolder.INSTANCE.getStatus());
        assertTrue(requests.isEmpty());
    }

    @Test
    public void whenThereIsNoKeyThenNothingIsSent() {
        // given
        configure(false);

        // when
        final var exception = assertThrows(AiException.class, () -> AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").build()));

        // then
        assertEquals(Reason.NO_KEY, exception.getReason());
        assertEquals(AiStatus.NO_KEY, AiClientHolder.INSTANCE.getStatus());
        assertFalse(AiClientHolder.INSTANCE.hasKey());
        assertTrue(requests.isEmpty());
    }

    @Test
    public void whenCallIsSentThenTheSecretsAreMaskedAndTheFallbackIsAskedFor() {
        // given
        respond(200, TEXT_ANSWER);

        // when
        final var answer = AiClientHolder.INSTANCE.send(AiCall.builder()
            .prompt("Why does jdbc:mysql://db?password=hunter22 fail?")
            .system("Never print hunter22.")
            .effort("HIGH")
            .build());

        // then the answer is the text of every block
        assertEquals("Hello there", answer.text());
        assertEquals("claude-opus-5-5", answer.model());
        assertEquals(12, answer.inputTokens());
        assertEquals(3, answer.outputTokens());
        assertEquals("end_turn", answer.stopReason());
        assertFalse(answer.isTruncated());
        assertEquals(AiStatus.READY, AiClientHolder.INSTANCE.getStatus());

        // and what went out holds no secret
        final var request = requests.getFirst();
        assertEquals("POST", request.method());
        assertEquals("/v1/messages", request.path());
        assertFalse(request.body().contains("hunter22"));
        assertTrue(request.body().contains("password=********"));
        assertTrue(request.body().contains("Never print ********."));
        assertTrue(request.body().contains("\"model\":\"claude-opus-5-5\""));
        assertTrue(request.body().contains("\"max_tokens\":16000"));
        assertTrue(request.body().contains("\"effort\":\"high\""));
        assertTrue(request.body().contains("\"fallbacks\":\"default\""));
        assertTrue(request.betaHeader().contains(AiClientHolder.FALLBACK_BETA));
        assertEquals("Bearer sk-ant-test", request.authorization());
    }

    @Test
    public void whenCallNamesAModelThenThatModelIsAsked() {
        // given
        respond(200, TEXT_ANSWER);
        model.set("  ");

        // when the settings have no model, but the call has one
        AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").model("claude-haiku-5-5").maxTokens(256L).build());
        respond(200, TEXT_ANSWER);
        AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").build());

        // then
        assertTrue(requests.get(0).body().contains("\"model\":\"claude-haiku-5-5\""));
        assertTrue(requests.get(0).body().contains("\"max_tokens\":256"));
        assertTrue(requests.get(1).body().contains("\"model\":\"" + AiClientHolder.DEFAULT_MODEL + "\""));
    }

    @Test
    public void whenModelDoesNotTakeAFallbackThenItIsAskedAgainWithoutOne() {
        // given
        respond(400, """
            {"type":"error","error":{"type":"invalid_request_error","message":"fallbacks: not supported for this model"}}""");
        respond(200, TEXT_ANSWER);

        // when
        final var answer = AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").build());

        // then
        assertEquals("Hello there", answer.text());
        assertEquals(2, requests.size());
        assertTrue(requests.get(0).body().contains("\"fallbacks\""));
        assertFalse(requests.get(1).body().contains("\"fallbacks\""));
        assertFalse(requests.get(1).betaHeader().contains(AiClientHolder.FALLBACK_BETA));
    }

    @Test
    public void whenClaudeDeclinesThenTheReasonIsTold() {
        // given
        respond(200, """
            {"id":"msg_2","type":"message","role":"assistant","model":"claude-opus-5-5","content":[],
             "stop_reason":"refusal","stop_details":{"type":"refusal","category":"cyber","explanation":"Not this one."},
             "stop_sequence":null,"usage":{"input_tokens":5,"output_tokens":0}}""");

        // when
        final var exception = assertThrows(AiException.class, () -> AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").build()));

        // then
        assertEquals(Reason.REFUSED, exception.getReason());
        assertEquals("Category: cyber. Not this one.", exception.getDetail());
    }

    @Test
    public void whenAnthropicSaysNoThenTheReasonIsTold() {
        assertReason(401, "authentication_error", Reason.KEY_REJECTED);
        assertReason(404, "not_found_error", Reason.MODEL_UNKNOWN);
        assertReason(429, "rate_limit_error", Reason.RATE_LIMITED);
        assertReason(500, "api_error", Reason.FAILED);
    }

    @Test
    public void whenAnswerTakesTooLongThenTheCallIsGivenUp() {
        // given
        responses.add(new Response(200, TEXT_ANSWER, Duration.ofSeconds(5)));

        // when
        final var exception = assertThrows(AiException.class,
            () -> AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").timeout(Duration.ofMillis(200)).build()));

        // then
        assertEquals(Reason.TIMEOUT, exception.getReason());
    }

    @Test
    public void whenWaitingThreadIsInterruptedThenTheCallIsCancelled() throws InterruptedException {
        // given
        responses.add(new Response(200, TEXT_ANSWER, Duration.ofSeconds(5)));
        final var failure = new AtomicReference<AiException>();
        final var interruptedAfterwards = new AtomicBoolean();
        final var thread = Thread.ofPlatform().start(() -> {
            try {
                AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").build());
            } catch (AiException exception) {
                failure.set(exception);
                interruptedAfterwards.set(Thread.currentThread().isInterrupted());
            }
        });

        // when a run is cancelled while it waits for Claude
        while (requests.isEmpty()) {
            Thread.onSpinWait();
        }
        thread.interrupt();
        thread.join(Duration.ofSeconds(5));

        // then
        assertEquals(Reason.CANCELLED, failure.get().getReason());
        assertTrue(interruptedAfterwards.get());
    }

    @Test
    public void whenConnectionIsTestedThenTheModelIsAskedAboutEvenWhileClaudeIsOff() {
        // given
        enabled.set(false);
        respond(200, """
            {"type":"model","id":"claude-opus-5-5","display_name":"Claude Opus 5.5","created_at":"2026-09-01T00:00:00Z"}""");

        // when
        final var check = AiClientHolder.INSTANCE.test();

        // then
        assertEquals("claude-opus-5-5", check.model());
        assertEquals("Claude Opus 5.5", check.displayName());
        assertEquals("GET", requests.getFirst().method());
        assertEquals("/v1/models/claude-opus-5-5", requests.getFirst().path());
    }

    @Test
    public void whenConnectionIsTestedWithoutAKeyThenNothingIsSent() {
        // given
        configure(false);

        // when
        final var exception = assertThrows(AiException.class, AiClientHolder.INSTANCE::test);

        // then
        assertEquals(Reason.NO_KEY, exception.getReason());
        assertTrue(requests.isEmpty());
    }

    @Test
    public void whenClientIsNeededSeveralTimesThenItIsBuiltOnce() {
        // given
        respond(200, TEXT_ANSWER);
        respond(200, TEXT_ANSWER);

        // when
        AiClientHolder.INSTANCE.getStatus();
        AiClientHolder.INSTANCE.send(AiCall.builder().prompt("one").build());
        AiClientHolder.INSTANCE.send(AiCall.builder().prompt("two").build());

        // then
        assertEquals(1, clientsBuilt.get());

        // and a new configuration builds it again
        configure(true);
        AiClientHolder.INSTANCE.getStatus();
        assertEquals(2, clientsBuilt.get());
    }

    private void assertReason(int status, String type, Reason reason) {
        requests.clear();
        respond(status, "{\"type\":\"error\",\"error\":{\"type\":\"" + type + "\",\"message\":\"no\"}}");
        final var exception = assertThrows(AiException.class, () -> AiClientHolder.INSTANCE.send(AiCall.builder().prompt("hi").build()));
        assertEquals(reason, exception.getReason(), "for " + status);
        assertEquals(1, requests.size(), "for " + status);
    }

    private void configure(boolean withKey) {
        AiClientHolder.INSTANCE.configure(enabled::get, model::get, () -> List.of("hunter22"), () -> {
            if (!withKey) {
                return Optional.empty();
            }
            clientsBuilt.incrementAndGet();
            return Optional.of(AnthropicOkHttpClient.builder()
                .apiKey("sk-ant-test")
                .baseUrl("http://" + server.getAddress().getHostString() + ":" + server.getAddress().getPort())
                .maxRetries(0)
                .build());
        });
    }

    private void respond(int status, String body) {
        synchronized (responses) {
            responses.add(new Response(status, body, Duration.ZERO));
        }
    }

    private void handle(HttpExchange exchange) throws IOException {
        final var body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
        requests.add(new Request(exchange.getRequestMethod(), exchange.getRequestURI().getPath(), body,
            String.valueOf(exchange.getRequestHeaders().get("anthropic-beta")), exchange.getRequestHeaders().getFirst("Authorization")));
        final Response response;
        synchronized (responses) {
            response = responses.poll();
        }
        try {
            if (response == null) {
                send(exchange, 500, "{\"type\":\"error\",\"error\":{\"type\":\"api_error\",\"message\":\"no answer prepared\"}}");
                return;
            }
            Thread.sleep(response.delay());
            send(exchange, response.status(), response.body());
        } catch (InterruptedException | IOException exception) {
            exchange.close();
        }
    }

    private static void send(HttpExchange exchange, int status, String body) throws IOException {
        final var bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        try (var output = exchange.getResponseBody()) {
            output.write(bytes);
        }
    }

    private record Request(String method, String path, String body, String betaHeader, String authorization) {

    }

    private record Response(int status, String body, Duration delay) {

    }

}
