package org.myworkflows.holder;

import com.anthropic.client.okhttp.AnthropicOkHttpClient;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.UncheckedIOException;
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
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * A server which plays Anthropic for {@link AiClientHolder}: it answers what it was told to, in that order, and keeps
 * what it was sent. Once it is connected, Claude is turned on, with a key, the model {@code claude-opus-5-5} and the
 * secret {@code hunter22}; closing it turns Claude off again.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class AnthropicStub implements AutoCloseable {

    public static final String SECRET = "hunter22";

    private static final String NO_ANSWER = "{\"type\":\"error\",\"error\":{\"type\":\"api_error\",\"message\":\"no answer prepared\"}}";

    private final List<Request> requests = new CopyOnWriteArrayList<>();
    private final Queue<Response> responses = new ArrayDeque<>();
    private final HttpServer server;

    private volatile boolean closed;

    private volatile Response defaultResponse = new Response(500, NO_ANSWER, Duration.ZERO, false);

    private AnthropicStub() {
        try {
            server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
        } catch (IOException exception) {
            throw new UncheckedIOException(exception);
        }
        server.setExecutor(Executors.newCachedThreadPool());
        server.createContext("/", this::handle);
        server.start();
    }

    public static AnthropicStub connect() {
        final var stub = new AnthropicStub();
        AiClientHolder.INSTANCE.configure(() -> true, () -> AiClientHolder.DEFAULT_MODEL, () -> List.of(SECRET),
            () -> Optional.of(AnthropicOkHttpClient.builder()
                .apiKey("sk-ant-test")
                .baseUrl("http://" + stub.server.getAddress().getHostString() + ":" + stub.server.getAddress().getPort())
                .maxRetries(0)
                .build()));
        return stub;
    }

    /**
     * @return a message of Claude which holds the text, ended as the stop reason says.
     */
    public static String message(String text, String stopReason) {
        return """
            {"id":"msg_1","type":"message","role":"assistant","model":"claude-opus-5-5",
             "content":[{"type":"text","text":%s}],
             "stop_reason":"%s","stop_sequence":null,"usage":{"input_tokens":12,"output_tokens":34}}"""
            .formatted(quote(text), stopReason).replace("\n", "");
    }

    public static String batch(String status) {
        return """
            {"id":"msgbatch_1","type":"message_batch","processing_status":"%s",
             "request_counts":{"processing":0,"succeeded":0,"errored":0,"canceled":0,"expired":0},
             "ended_at":null,"created_at":"2026-11-09T00:00:00Z","expires_at":"2026-11-10T00:00:00Z",
             "archived_at":null,"cancel_initiated_at":null,"results_url":null}""".formatted(status).replace("\n", "");
    }

    public static String succeeded(int index, String message) {
        return "{\"custom_id\":\"" + AiClientHolder.BATCH_ID_PREFIX + index + "\",\"result\":{\"type\":\"succeeded\",\"message\":" + message + "}}";
    }

    public static String errored(int index, String errorMessage) {
        return "{\"custom_id\":\"" + AiClientHolder.BATCH_ID_PREFIX + index + "\",\"result\":{\"type\":\"errored\",\"error\":"
            + "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":" + quote(errorMessage) + "}}}}";
    }

    public AnthropicStub respond(int status, String body) {
        return add(new Response(status, body, Duration.ZERO, false));
    }

    public AnthropicStub respondLines(String... lines) {
        return add(new Response(200, String.join("\n", lines) + "\n", Duration.ZERO, true));
    }

    public AnthropicStub respondAfter(Duration delay, String body) {
        return add(new Response(200, body, delay, false));
    }

    /**
     * Answers with this once there is nothing left to answer with.
     */
    public AnthropicStub respondByDefault(String body) {
        defaultResponse = new Response(200, body, Duration.ZERO, false);
        return this;
    }

    public List<Request> requests() {
        return requests;
    }

    /**
     * Waits a while for a request which may still be on its way, as one which nobody waits for is.
     */
    public Request awaitRequest(Predicate<Request> predicate) {
        final var deadline = System.nanoTime() + Duration.ofSeconds(5).toNanos();
        while (System.nanoTime() < deadline) {
            final var found = requests.stream().filter(predicate).findFirst();
            if (found.isPresent()) {
                return found.get();
            }
            Thread.onSpinWait();
        }
        return fail("No such request came: " + requests);
    }

    @Override
    public void close() {
        if (closed) {
            return;
        }
        closed = true;
        AiClientHolder.INSTANCE.configure(() -> false, () -> AiClientHolder.DEFAULT_MODEL, List::of, Optional::empty);
        server.stop(0);
    }

    private AnthropicStub add(Response response) {
        synchronized (responses) {
            responses.add(response);
        }
        return this;
    }

    private void handle(HttpExchange exchange) throws IOException {
        final var body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
        requests.add(new Request(exchange.getRequestMethod(), exchange.getRequestURI().getPath(), body));
        final Response response;
        synchronized (responses) {
            response = Optional.ofNullable(responses.poll()).orElse(defaultResponse);
        }
        try {
            Thread.sleep(response.delay());
            final var bytes = response.body().getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", response.lines() ? "application/x-jsonl" : "application/json");
            exchange.sendResponseHeaders(response.status(), bytes.length);
            try (var output = exchange.getResponseBody()) {
                output.write(bytes);
            }
        } catch (InterruptedException | IOException exception) {
            exchange.close();
        }
    }

    private static String quote(String text) {
        return "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\"";
    }

    public record Request(String method, String path, String body) {

    }

    private record Response(int status, String body, Duration delay, boolean lines) {

    }

}
