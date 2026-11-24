package org.myworkflows.holder;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.jspecify.annotations.Nullable;
import org.myworkflows.domain.ai.AiAnswer;
import org.myworkflows.domain.ai.AiBatchResult;
import org.myworkflows.domain.ai.AiCall;
import org.myworkflows.domain.ai.AiConnectionCheck;
import org.myworkflows.domain.ai.AiStatus;
import org.myworkflows.exception.AiException;
import org.myworkflows.exception.AiException.Reason;
import org.myworkflows.holder.AnthropicClient.ApiException;
import org.myworkflows.util.SecretMasker;
import tools.jackson.databind.JsonNode;

import java.io.IOException;
import java.time.Duration;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * The one way out to Claude. A command cannot reach a Spring bean, so the client lives here, like the cipher of
 * {@link EncryptionHolder}; {@code AiConfig} tells it, at startup, how to read whether Claude is turned on, the model
 * of the settings and the secrets to mask. Each of them is read on every call, so that a change takes effect at once.
 *
 * <p>Nothing is sent unless an admin turned Claude on, on the features page, and the server has an API key: the
 * {@code ANTHROPIC_API_KEY} environment variable, never a setting, as the settings are plain text in the database. The
 * prompt and the system text of a call are masked first (see {@link SecretMasker#maskText}). A request which Claude
 * declines is run again on the model Anthropic recommends for its reason, on Anthropic's side ({@code fallbacks}) - but
 * not in a batch, which does not take them. What a request holds is written here, and sent by {@link AnthropicClient}.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Slf4j
public enum AiClientHolder {
    INSTANCE;

    public static final String API_KEY_VARIABLE = "ANTHROPIC_API_KEY";

    public static final String DEFAULT_MODEL = "claude-haiku-5-5";

    /**
     * Enough for a long answer without streaming it, which keeps a call below the timeouts of the HTTP client.
     */
    public static final long DEFAULT_MAX_TOKENS = 16_000L;

    public static final Duration DEFAULT_TIMEOUT = Duration.ofMinutes(5);

    /**
     * Anthropic gives a batch up to a day, after which what is left of it expires.
     */
    public static final Duration DEFAULT_BATCH_TIMEOUT = Duration.ofHours(24);

    public static final Duration DEFAULT_BATCH_POLL_INTERVAL = Duration.ofSeconds(30);

    static final String FALLBACK_BETA = "server-side-fallback-2026-07-01";
    static final String FALLBACKS = "fallbacks";
    static final String FALLBACKS_DEFAULT = "default";

    static final String BATCH_ID_PREFIX = "item-";

    private static final String MESSAGES_PATH = "/v1/messages";
    private static final String BATCHES_PATH = MESSAGES_PATH + "/batches";
    private static final String MODELS_PATH = "/v1/models";

    private static final String BETA_HEADER = "anthropic-beta";

    private static final String ENDED = "ended";
    private static final String REFUSAL = "refusal";

    private static final String ADDITIONAL_PROPERTIES = "additionalProperties";
    private static final String OBJECT = "object";
    private static final String PROPERTIES = "properties";
    private static final String TYPE = "type";

    // the keywords of a JSON schema whose value is a schema, a map of schemas or a list of schemas
    private static final Set<String> SCHEMA_KEYWORDS = Set.of("items", ADDITIONAL_PROPERTIES, "not", "contains", "if", "then", "else");
    private static final Set<String> SCHEMA_MAP_KEYWORDS = Set.of(PROPERTIES, "$defs", "definitions", "patternProperties");
    private static final Set<String> SCHEMA_LIST_KEYWORDS = Set.of("anyOf", "allOf", "oneOf", "prefixItems");

    private final ReentrantLock lock = new ReentrantLock();

    private volatile Settings settings = Settings.NONE;

    // built the first time it is needed, and only when there is a key; guarded by the lock
    @SuppressWarnings("OptionalUsedAsFieldOrParameterType")
    private Optional<AnthropicClient> client;

    /**
     * @param enabled whether an admin turned Claude on.
     * @param defaultModel the model of the settings, asked when a call names none.
     * @param secrets the values which are known to be secrets, masked wherever they stand in what is sent.
     * @param clientFactory builds the client, or nothing when there is no key; called once, the first time a client is
     *     needed after this.
     */
    public void configure(BooleanSupplier enabled, Supplier<String> defaultModel, Supplier<? extends Collection<String>> secrets,
                          Supplier<Optional<AnthropicClient>> clientFactory) {
        lock.lock();
        try {
            closeClient();
            settings = new Settings(enabled, defaultModel, secrets, clientFactory);
        } finally {
            lock.unlock();
        }
    }

    /**
     * @return the client which sends the key of {@code ANTHROPIC_API_KEY}, or nothing when that variable is not set.
     */
    public static Optional<AnthropicClient> createFromEnvironment() {
        return AnthropicClient.fromEnvironment();
    }

    /**
     * @return whether the server has an API key, whether Claude is turned on or not.
     */
    public boolean hasKey() {
        return findClient().isPresent();
    }

    public AiStatus getStatus() {
        if (!settings.enabled().getAsBoolean()) {
            return AiStatus.DISABLED;
        }
        return findClient().isPresent() ? AiStatus.READY : AiStatus.NO_KEY;
    }

    /**
     * @return the model asked when a call names none: the one of the settings, or {@link #DEFAULT_MODEL} without one.
     */
    public String getDefaultModel() {
        return Optional.ofNullable(settings.defaultModel().get())
            .map(String::strip)
            .filter(StringUtils::isNotEmpty)
            .orElse(DEFAULT_MODEL);
    }

    /**
     * Asks Claude, on the thread of the caller, which waits for the answer. An interrupt - a run which is cancelled -
     * gives the call up at once.
     *
     * @throws AiException when Claude cannot be asked, or does not answer.
     */
    public AiAnswer send(AiCall call) {
        final var anthropicClient = requireClient();
        final var model = Optional.ofNullable(call.model()).filter(StringUtils::isNotBlank).map(String::strip).orElseGet(this::getDefaultModel);
        final var params = toParams(call, model, settings.secrets().get());
        final var timeout = Optional.ofNullable(call.timeout()).orElse(DEFAULT_TIMEOUT);
        try {
            return toAnswer(await(() -> anthropicClient.post(MESSAGES_PATH, withFallbacks(params), Map.of(BETA_HEADER, FALLBACK_BETA), timeout),
                timeout, model));
        } catch (AiException exception) {
            if (isFallbackRefused(exception)) {
                // a model which does not take the fallbacks is asked again without them, as if they did not exist
                log.info("The model '{}' does not take a fallback, so it is asked without one.", model);
                return toAnswer(await(() -> anthropicClient.post(MESSAGES_PATH, params, Map.of(), timeout), timeout, model));
            }
            throw exception;
        }
    }

    /**
     * Asks Claude several questions at once, through the Message Batches API: half the price of as many calls, but the
     * answers come when Anthropic is done with all of them - often within minutes, at most within a day. The thread of
     * the caller waits for them and asks how the batch stands every {@link #DEFAULT_BATCH_POLL_INTERVAL}. An interrupt -
     * a run which is cancelled - or a batch which outlasts the timeout cancels the batch on Anthropic's side too.
     *
     * @param calls the questions, each with its prompt; the timeout of a call does not count here.
     * @param timeout how long the whole batch may take; {@link #DEFAULT_BATCH_TIMEOUT} when it is not given.
     * @return what became of every call, in the order of the calls. A call which was not answered - an error, or a
     *     refusal, which a batch does not fall back from - fails alone.
     * @throws AiException when Claude cannot be asked, or the batch does not end in time.
     */
    public List<AiBatchResult> sendBatch(List<AiCall> calls, @Nullable Duration timeout) {
        return sendBatch(calls, timeout, DEFAULT_BATCH_POLL_INTERVAL);
    }

    public List<AiBatchResult> sendBatch(List<AiCall> calls, @Nullable Duration timeout, Duration pollInterval) {
        if (calls.isEmpty()) {
            return List.of();
        }
        final var anthropicClient = requireClient();
        final var total = Optional.ofNullable(timeout).orElse(DEFAULT_BATCH_TIMEOUT);
        final var deadline = System.nanoTime() + total.toNanos();
        final var defaultModel = getDefaultModel();
        final var body = toBatchParams(calls, defaultModel, settings.secrets().get());
        var batch = await(() -> anthropicClient.post(BATCHES_PATH, body, Map.of(), total), total, defaultModel);
        final var batchId = Optional.ofNullable(textOf(batch.path("id")))
            .orElseThrow(() -> new AiException(Reason.FAILED, "Anthropic created the batch without an id."));
        final var batchPath = BATCHES_PATH + "/" + AnthropicClient.pathSegment(batchId);
        log.info("Batch '{}' of {} calls to Claude was created.", batchId, calls.size());
        try {
            while (!ENDED.equals(textOf(batch.path("processing_status")))) {
                final var left = Duration.ofNanos(deadline - System.nanoTime());
                if (left.isNegative() || left.isZero()) {
                    throw new AiException(Reason.TIMEOUT, total.toString());
                }
                pause(pollInterval.compareTo(left) < 0 ? pollInterval : left);
                final var stillLeft = Duration.ofNanos(Math.max(1L, deadline - System.nanoTime()));
                batch = await(() -> anthropicClient.get(batchPath, stillLeft), total, defaultModel);
            }
        } catch (AiException exception) {
            if (exception.getReason() == Reason.TIMEOUT || exception.getReason() == Reason.CANCELLED) {
                cancel(anthropicClient, batchId, batchPath);
            }
            throw exception;
        }
        return readResults(anthropicClient, batchPath, calls.size());
    }

    /**
     * Asks Anthropic about the model of the settings, which uses no tokens: an answer means that the key is taken and
     * the model is known. It works while Claude is turned off, so that an admin can try the key before turning it on:
     * nothing is sent but the name of the model.
     *
     * @throws AiException when there is no key, or Anthropic says no.
     */
    public AiConnectionCheck test() {
        final var anthropicClient = findClient().orElseThrow(() -> new AiException(Reason.NO_KEY));
        final var model = getDefaultModel();
        final var start = System.nanoTime();
        final var info = await(() -> anthropicClient.get(MODELS_PATH + "/" + AnthropicClient.pathSegment(model), AnthropicClient.REQUEST_TIMEOUT),
            AnthropicClient.REQUEST_TIMEOUT, model);
        return new AiConnectionCheck(textOf(info.path("id")), textOf(info.path("display_name")), Duration.ofNanos(System.nanoTime() - start));
    }

    /**
     * Lets the client go, which a {@link #configure} or the next call builds again.
     */
    public void close() {
        lock.lock();
        try {
            closeClient();
        } finally {
            lock.unlock();
        }
    }

    /**
     * @return the body of a message: without the fallbacks, which only a call made alone asks for (see
     *     {@link #withFallbacks}).
     */
    static Map<String, Object> toParams(AiCall call, String model, Collection<String> secrets) {
        final var params = new LinkedHashMap<String, Object>();
        params.put("model", model);
        params.put("max_tokens", Optional.ofNullable(call.maxTokens()).orElse(DEFAULT_MAX_TOKENS));
        params.put("messages", List.of(Map.of("role", "user", "content", SecretMasker.maskText(call.prompt(), secrets))));
        if (StringUtils.isNotBlank(call.system())) {
            params.put("system", SecretMasker.maskText(call.system(), secrets));
        }
        toOutputConfig(call).ifPresent(outputConfig -> params.put("output_config", outputConfig));
        return params;
    }

    static Map<String, Object> withFallbacks(Map<String, Object> params) {
        final var withFallbacks = new LinkedHashMap<>(params);
        withFallbacks.put(FALLBACKS, FALLBACKS_DEFAULT);
        return withFallbacks;
    }

    /**
     * @return one request per call, each of them named after the index of its call. A batch takes no fallbacks.
     */
    static Map<String, Object> toBatchParams(List<AiCall> calls, String defaultModel, Collection<String> secrets) {
        final var requests = IntStream.range(0, calls.size())
            .mapToObj(index -> {
                final var call = calls.get(index);
                final var model = Optional.ofNullable(call.model()).filter(StringUtils::isNotBlank).map(String::strip).orElse(defaultModel);
                return Map.of("custom_id", BATCH_ID_PREFIX + index, "params", toParams(call, model, secrets));
            })
            .toList();
        return Map.of("requests", requests);
    }

    static Optional<Map<String, Object>> toOutputConfig(AiCall call) {
        if (call.effort() == null && !call.isStructured()) {
            return Optional.empty();
        }
        final var outputConfig = new LinkedHashMap<String, Object>();
        if (call.effort() != null) {
            outputConfig.put("effort", call.effort());
        }
        if (call.isStructured()) {
            outputConfig.put("format", toFormat(call.schema()));
        }
        return Optional.of(outputConfig);
    }

    /**
     * @return the format which holds an answer to the schema. Anthropic takes an object only when it says that no
     *     other field may be added ({@code additionalProperties: false}), so every object of the schema which says
     *     nothing about it says that.
     */
    static Map<String, Object> toFormat(Map<String, ?> schema) {
        final var format = new LinkedHashMap<String, Object>();
        format.put("schema", closeObjects(schema));
        format.put(TYPE, "json_schema");
        return format;
    }

    static Map<String, Object> closeObjects(Map<?, ?> schema) {
        final var closedSchema = new LinkedHashMap<String, Object>();
        schema.forEach((key, value) -> {
            final var keyword = String.valueOf(key);
            closedSchema.put(keyword, closeObjectsUnder(keyword, value));
        });
        if (isObject(closedSchema) && !closedSchema.containsKey(ADDITIONAL_PROPERTIES)) {
            closedSchema.put(ADDITIONAL_PROPERTIES, false);
        }
        return closedSchema;
    }

    /**
     * @param result the {@code result} of a line of the results of a batch.
     */
    static AiBatchResult toBatchResult(JsonNode result) {
        final var type = textOf(result.path(TYPE));
        if ("succeeded".equals(type)) {
            try {
                return AiBatchResult.answered(toAnswer(result.path("message")));
            } catch (AiException exception) {
                return AiBatchResult.failed(exception.getMessage());
            }
        }
        final var detail = switch (StringUtils.defaultString(type)) {
            case "errored" -> StringUtils.defaultIfBlank(textOf(result.path("error").path("error").path("message")), "an error without a message.");
            case "canceled" -> "the batch was cancelled before this call was answered.";
            default -> "the batch expired before this call was answered.";
        };
        return AiBatchResult.failed(new AiException(Reason.FAILED, detail).getMessage());
    }

    /**
     * @param message a message of the API, as JSON.
     */
    static AiAnswer toAnswer(JsonNode message) {
        final var stopReason = textOf(message.path("stop_reason"));
        if (REFUSAL.equals(stopReason)) {
            throw new AiException(Reason.REFUSED, describeRefusal(message.path("stop_details")));
        }
        final var text = message.path("content").values().stream()
            .filter(block -> "text".equals(textOf(block.path(TYPE))))
            .map(block -> StringUtils.defaultString(textOf(block.path("text"))))
            .collect(Collectors.joining());
        final var usage = message.path("usage");
        return new AiAnswer(text, textOf(message.path("model")), usage.path("input_tokens").asLong(0L),
            usage.path("output_tokens").asLong(0L), stopReason);
    }

    static AiException toAiException(Throwable failure, @Nullable String model) {
        var cause = failure;
        while ((cause instanceof CompletionException || cause instanceof ExecutionException) && cause.getCause() != null) {
            cause = cause.getCause();
        }
        return switch (cause) {
            case AiException aiException -> aiException;
            case ApiException exception -> switch (exception.getStatus()) {
                case 401, 403 -> new AiException(Reason.KEY_REJECTED, null, exception);
                case 404 -> new AiException(Reason.MODEL_UNKNOWN, model, exception);
                case 429 -> new AiException(Reason.RATE_LIMITED, null, exception);
                default -> new AiException(Reason.FAILED, exception.getMessage(), exception);
            };
            case IOException exception -> new AiException(Reason.UNREACHABLE, null, exception);
            default -> new AiException(Reason.FAILED, cause.getMessage(), cause);
        };
    }

    private static Object closeObjectsUnder(String keyword, Object value) {
        if (SCHEMA_KEYWORDS.contains(keyword) && value instanceof Map<?, ?> schema) {
            return closeObjects(schema);
        }
        if (SCHEMA_MAP_KEYWORDS.contains(keyword) && value instanceof Map<?, ?> schemas) {
            final var closedSchemas = new LinkedHashMap<String, Object>();
            schemas.forEach((name, schema) -> closedSchemas.put(String.valueOf(name), schema instanceof Map<?, ?> map ? closeObjects(map) : schema));
            return closedSchemas;
        }
        if (SCHEMA_LIST_KEYWORDS.contains(keyword) && value instanceof List<?> schemas) {
            return schemas.stream().map(schema -> schema instanceof Map<?, ?> map ? closeObjects(map) : schema).toList();
        }
        return value;
    }

    private static boolean isObject(Map<String, Object> schema) {
        final var type = schema.get(TYPE);
        return OBJECT.equals(type) || type instanceof Collection<?> types && types.contains(OBJECT) || schema.containsKey(PROPERTIES);
    }

    /**
     * @param timeout the timeout which the failure names: the one of the call, or of the whole batch.
     */
    private static JsonNode await(Exchange exchange, Duration timeout, @Nullable String model) {
        try {
            return exchange.run();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new AiException(Reason.CANCELLED, null, exception);
        } catch (TimeoutException exception) {
            throw new AiException(Reason.TIMEOUT, timeout.toString(), exception);
        } catch (IOException | RuntimeException exception) {
            throw toAiException(exception, model);
        }
    }

    private static void pause(Duration duration) {
        try {
            Thread.sleep(duration);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new AiException(Reason.CANCELLED, null, exception);
        }
    }

    private static void cancel(AnthropicClient anthropicClient, String batchId, String batchPath) {
        // what is left of the batch would be paid for, and nobody would read it
        anthropicClient.postAsync(batchPath + "/cancel").whenComplete((_, failure) -> {
            if (failure == null) {
                log.info("Batch '{}' of calls to Claude was cancelled.", batchId);
            } else {
                log.warn("Batch '{}' of calls to Claude could not be cancelled.", batchId, failure);
            }
        });
    }

    private static List<AiBatchResult> readResults(AnthropicClient anthropicClient, String batchPath, int size) {
        final var results = new HashMap<String, JsonNode>();
        final List<JsonNode> lines;
        try {
            lines = anthropicClient.getLines(batchPath + "/results", AnthropicClient.REQUEST_TIMEOUT);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new AiException(Reason.CANCELLED, null, exception);
        } catch (TimeoutException exception) {
            throw new AiException(Reason.TIMEOUT, AnthropicClient.REQUEST_TIMEOUT.toString(), exception);
        } catch (IOException | RuntimeException exception) {
            throw toAiException(exception, null);
        }
        lines.forEach(line -> results.put(textOf(line.path("custom_id")), line.path("result")));
        return IntStream.range(0, size)
            .mapToObj(index -> Optional.ofNullable(results.get(BATCH_ID_PREFIX + index))
                .map(AiClientHolder::toBatchResult)
                .orElseGet(() -> AiBatchResult.failed(new AiException(Reason.FAILED, "the batch has no result for this call.").getMessage())))
            .toList();
    }

    private static boolean isFallbackRefused(AiException exception) {
        return exception.getReason() == Reason.FAILED && exception.getCause() instanceof ApiException apiException
            && apiException.getStatus() == 400 && Strings.CI.contains(exception.getMessage(), "fallback");
    }

    private static String describeRefusal(JsonNode details) {
        final var category = textOf(details.path("category"));
        final var parts = List.of(
            category == null ? StringUtils.EMPTY : "Category: " + category + ".",
            StringUtils.defaultString(textOf(details.path("explanation"))));
        return parts.stream().filter(StringUtils::isNotBlank).collect(Collectors.joining(" "));
    }

    /**
     * @return the text of a node, or {@code null} for one which is missing, {@code null} or no text.
     */
    @Nullable
    private static String textOf(JsonNode node) {
        return node.isString() ? node.stringValue() : null;
    }

    private AnthropicClient requireClient() {
        if (!settings.enabled().getAsBoolean()) {
            throw new AiException(Reason.DISABLED);
        }
        return findClient().orElseThrow(() -> new AiException(Reason.NO_KEY));
    }

    private Optional<AnthropicClient> findClient() {
        lock.lock();
        try {
            if (client == null) {
                client = settings.clientFactory().get();
            }
            return client;
        } finally {
            lock.unlock();
        }
    }

    private void closeClient() {
        if (client != null) {
            client.ifPresent(AnthropicClient::close);
            client = null;
        }
    }

    @FunctionalInterface
    private interface Exchange {

        JsonNode run() throws IOException, InterruptedException, TimeoutException;

    }

    private record Settings(BooleanSupplier enabled, Supplier<String> defaultModel, Supplier<? extends Collection<String>> secrets,
                            Supplier<Optional<AnthropicClient>> clientFactory) {

        static final Settings NONE = new Settings(() -> false, () -> DEFAULT_MODEL, List::of, Optional::empty);

    }

}
