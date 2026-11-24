package org.myworkflows.holder;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.jspecify.annotations.Nullable;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * The few calls of the Anthropic API which {@link AiClientHolder} makes, over the HTTP client of the JDK: a message, a
 * batch of them and its results, the cancel of a batch, and a model. It sends what it is given as JSON and gives back
 * the JSON of the answer, so that what a request holds stays the business of the holder.
 *
 * <p>A request is sent and waited for on the thread of the caller, within the time it is given: an interrupt gives it
 * up, and so does the end of that time. A request which failed in a way that may pass - no connection, {@code 408},
 * {@code 409}, {@code 429} or a {@code 5xx}, unless the server says not to try again - is sent again, at most
 * {@link #DEFAULT_MAX_RETRIES} times, after the delay the server asks for or a backoff of its own. Any other answer
 * than a {@code 2xx} is an {@link ApiException}.
 *
 * @author Mihai Surdeanu
 * @since 2.6
 */
public final class AnthropicClient {

    public static final String BASE_URL_VARIABLE = "ANTHROPIC_BASE_URL";

    public static final URI DEFAULT_BASE_URL = URI.create("https://api.anthropic.com");

    /**
     * The version of the API whose requests and answers this client writes and reads.
     */
    public static final String API_VERSION = "2026-10-21";

    public static final int DEFAULT_MAX_RETRIES = 2;

    /**
     * How long a request which is not given a time of its own may take: reading the results of a batch, or a model.
     */
    public static final Duration REQUEST_TIMEOUT = Duration.ofMinutes(10);

    static final Duration CONNECT_TIMEOUT = Duration.ofMinutes(1);

    private static final Duration MAX_BACKOFF = Duration.ofSeconds(8);

    private static final JsonMapper JSON_MAPPER = JsonMapper.builder().build();

    private final String apiKey;
    private final URI baseUrl;
    private final int maxRetries;
    private final HttpClient httpClient;

    public AnthropicClient(String apiKey, URI baseUrl, int maxRetries) {
        this.apiKey = apiKey;
        this.baseUrl = baseUrl;
        this.maxRetries = maxRetries;
        this.httpClient = HttpClient.newBuilder()
            .connectTimeout(CONNECT_TIMEOUT)
            .version("http".equalsIgnoreCase(baseUrl.getScheme()) ? HttpClient.Version.HTTP_1_1 : HttpClient.Version.HTTP_2)
            .build();
    }

    /**
     * @return the client of the key in {@code ANTHROPIC_API_KEY} - sent to {@code ANTHROPIC_BASE_URL} when that one is
     *     set too, such as a proxy -, or nothing when there is no key.
     */
    public static Optional<AnthropicClient> fromEnvironment() {
        final var apiKey = System.getenv(AiClientHolder.API_KEY_VARIABLE);
        if (StringUtils.isBlank(apiKey)) {
            return Optional.empty();
        }
        final var baseUrl = Optional.ofNullable(System.getenv(BASE_URL_VARIABLE))
            .filter(StringUtils::isNotBlank)
            .map(String::strip)
            .map(URI::create)
            .orElse(DEFAULT_BASE_URL);
        return Optional.of(new AnthropicClient(apiKey.strip(), baseUrl, DEFAULT_MAX_RETRIES));
    }

    /**
     * @return a segment of a path, such as the name of a model or the id of a batch, as it is written in a URL.
     */
    public static String pathSegment(String value) {
        return URLEncoder.encode(value, StandardCharsets.UTF_8).replace("+", "%20");
    }

    /**
     * @param headers what is sent besides the headers of every request, such as {@code anthropic-beta}.
     * @return the JSON of the answer.
     */
    public JsonNode post(String path, Object body, Map<String, String> headers, Duration wait)
        throws IOException, InterruptedException, TimeoutException {
        return JSON_MAPPER.readTree(send(newRequest(path, headers)
            .POST(HttpRequest.BodyPublishers.ofString(JSON_MAPPER.writeValueAsString(body), StandardCharsets.UTF_8))
            .build(), wait));
    }

    public JsonNode get(String path, Duration wait) throws IOException, InterruptedException, TimeoutException {
        return JSON_MAPPER.readTree(send(newRequest(path, Map.of()).GET().build(), wait));
    }

    /**
     * @return every line of an answer which is a JSON object per line (JSONL), such as the results of a batch.
     */
    public List<JsonNode> getLines(String path, Duration wait) throws IOException, InterruptedException, TimeoutException {
        return send(newRequest(path, Map.of()).GET().build(), wait).lines()
            .filter(StringUtils::isNotBlank)
            .map(JSON_MAPPER::readTree)
            .toList();
    }

    /**
     * Sends a request without a body, once, and does not wait for it: for what nobody waits for, such as the cancel of
     * a batch of a run which is cancelled itself.
     *
     * @return the answer, or the failure of the request: an {@link ApiException} for an answer which is not a
     *     {@code 2xx}.
     */
    public CompletableFuture<JsonNode> postAsync(String path) {
        return httpClient.sendAsync(newRequest(path, Map.of()).POST(HttpRequest.BodyPublishers.noBody()).build(),
                BodyHandlers.ofString(StandardCharsets.UTF_8))
            .thenApply(response -> JSON_MAPPER.readTree(bodyOf(response)));
    }

    /**
     * Gives up whatever is still on its way.
     */
    public void close() {
        httpClient.shutdownNow();
    }

    private HttpRequest.Builder newRequest(String path, Map<String, String> headers) {
        final var builder = HttpRequest.newBuilder(baseUrl.resolve(path))
            .timeout(REQUEST_TIMEOUT)
            .header("Authorization", "Bearer " + apiKey)
            .header("anthropic-version", API_VERSION)
            .header("Accept", "application/json")
            .header("Content-Type", "application/json");
        headers.forEach(builder::header);
        return builder;
    }

    private String send(HttpRequest request, Duration wait) throws IOException, InterruptedException, TimeoutException {
        final var deadline = System.nanoTime() + wait.toNanos();
        for (var attempt = 1; ; attempt++) {
            final var future = httpClient.sendAsync(request, BodyHandlers.ofString(StandardCharsets.UTF_8));
            HttpResponse<String> response = null;
            IOException failure = null;
            try {
                response = future.get(Math.max(0L, deadline - System.nanoTime()), TimeUnit.NANOSECONDS);
            } catch (InterruptedException | TimeoutException exception) {
                future.cancel(true);
                throw exception;
            } catch (ExecutionException exception) {
                failure = exception.getCause() instanceof IOException ioException ? ioException : new IOException(exception.getCause());
            }
            if (attempt > maxRetries || response != null && !shouldRetry(response)) {
                if (failure != null) {
                    throw failure;
                }
                return bodyOf(response);
            }
            final var left = deadline - System.nanoTime();
            if (left <= 0) {
                throw new TimeoutException();
            }
            Thread.sleep(Duration.ofNanos(Math.min(backoff(attempt, response).toNanos(), left)));
        }
    }

    private static String bodyOf(HttpResponse<String> response) {
        if (response.statusCode() / 100 != 2) {
            throw new ApiException(response.statusCode(), messageOf(response.body()),
                response.headers().firstValue("request-id").orElse(null));
        }
        return response.body();
    }

    private static boolean shouldRetry(HttpResponse<String> response) {
        final var shouldRetry = response.headers().firstValue("x-should-retry").orElse(null);
        if ("true".equals(shouldRetry) || "false".equals(shouldRetry)) {
            return Boolean.parseBoolean(shouldRetry);
        }
        final var status = response.statusCode();
        return status == 408 || status == 409 || status == 429 || status >= 500;
    }

    /**
     * @return the delay the server asks for ({@code retry-after-ms}, or {@code retry-after} in seconds), or else half
     *     a second doubled at every attempt, at most {@link #MAX_BACKOFF}, minus a quarter at most so that the clients
     *     which failed together do not come back together.
     */
    static Duration backoff(int attempt, @Nullable HttpResponse<?> response) {
        if (response != null) {
            final var asked = response.headers().firstValue("retry-after-ms").map(value -> parseDelay(value, 1L))
                .or(() -> response.headers().firstValue("retry-after").map(value -> parseDelay(value, 1_000L)))
                .orElse(0L);
            if (asked > 0) {
                return Duration.ofMillis(asked);
            }
        }
        final var seconds = Math.min(0.5 * Math.pow(2, attempt - 1), MAX_BACKOFF.toSeconds());
        final var jitter = 1.0 - 0.25 * ThreadLocalRandom.current().nextDouble();
        return Duration.ofMillis((long) (seconds * jitter * 1_000));
    }

    private static long parseDelay(String value, long millisPerUnit) {
        try {
            return (long) (Double.parseDouble(value.strip()) * millisPerUnit);
        } catch (NumberFormatException exception) {
            // a date, which is left to the backoff
            return 0L;
        }
    }

    private static String messageOf(String body) {
        try {
            final var message = JSON_MAPPER.readTree(body).path("error").path("message");
            if (message.isString() && StringUtils.isNotBlank(message.stringValue())) {
                return message.stringValue();
            }
        } catch (RuntimeException exception) {
            // not JSON: the body is the message
        }
        return StringUtils.defaultIfBlank(StringUtils.abbreviate(body, 500), "no message");
    }

    /**
     * An answer of Anthropic which is not a {@code 2xx}.
     *
     * @author Mihai Surdeanu
     * @since 2.6
     */
    @Getter
    public static final class ApiException extends RuntimeException {

        private final int status;

        @Nullable
        private final String requestId;

        public ApiException(int status, String message, @Nullable String requestId) {
            super(status + ": " + message + (requestId == null ? StringUtils.EMPTY : " (request " + requestId + ")"));
            this.status = status;
            this.requestId = requestId;
        }

    }

}
