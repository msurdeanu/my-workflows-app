package org.myworkflows.holder;

import com.anthropic.client.AnthropicClient;
import com.anthropic.client.okhttp.AnthropicOkHttpClient;
import com.anthropic.core.JsonValue;
import com.anthropic.errors.AnthropicIoException;
import com.anthropic.errors.AnthropicServiceException;
import com.anthropic.errors.BadRequestException;
import com.anthropic.errors.NotFoundException;
import com.anthropic.errors.PermissionDeniedException;
import com.anthropic.errors.RateLimitException;
import com.anthropic.errors.UnauthorizedException;
import com.anthropic.models.messages.JsonOutputFormat;
import com.anthropic.models.messages.Message;
import com.anthropic.models.messages.MessageCreateParams;
import com.anthropic.models.messages.OutputConfig;
import com.anthropic.models.messages.RefusalStopDetails;
import com.anthropic.models.messages.StopReason;
import com.anthropic.models.messages.TextBlock;
import com.anthropic.models.messages.batches.BatchCreateParams;
import com.anthropic.models.messages.batches.MessageBatch;
import com.anthropic.models.messages.batches.MessageBatchResult;
import com.anthropic.services.async.messages.BatchServiceAsync;
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
import org.myworkflows.util.SecretMasker;

import java.time.Duration;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
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
 * not in a batch, which does not take them.
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

    private static final String BETA_HEADER = "anthropic-beta";

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
     * @return the client which reads the key from {@code ANTHROPIC_API_KEY}, or nothing when that variable is not set.
     */
    public static Optional<AnthropicClient> createFromEnvironment() {
        if (StringUtils.isBlank(System.getenv(API_KEY_VARIABLE))) {
            return Optional.empty();
        }
        return Optional.of(AnthropicOkHttpClient.builder().fromEnv().build());
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
            return toAnswer(await(anthropicClient.async().messages().create(params), timeout, timeout, model));
        } catch (AiException exception) {
            if (isFallbackRefused(exception)) {
                // a model which does not take the fallbacks is asked again without them, as if they did not exist
                log.info("The model '{}' does not take a fallback, so it is asked without one.", model);
                return toAnswer(await(anthropicClient.async().messages().create(withoutFallbacks(params)), timeout, timeout, model));
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
        final var batches = anthropicClient.async().messages().batches();
        var batch = await(batches.create(toBatchParams(calls, defaultModel, settings.secrets().get())), total, total, defaultModel);
        log.info("Batch '{}' of {} calls to Claude was created.", batch.id(), calls.size());
        try {
            while (!MessageBatch.ProcessingStatus.ENDED.equals(batch.processingStatus())) {
                final var left = Duration.ofNanos(deadline - System.nanoTime());
                if (left.isNegative() || left.isZero()) {
                    throw new AiException(Reason.TIMEOUT, total.toString());
                }
                pause(pollInterval.compareTo(left) < 0 ? pollInterval : left);
                final var stillLeft = Duration.ofNanos(Math.max(1L, deadline - System.nanoTime()));
                batch = await(batches.retrieve(batch.id()), stillLeft, total, defaultModel);
            }
        } catch (AiException exception) {
            if (exception.getReason() == Reason.TIMEOUT || exception.getReason() == Reason.CANCELLED) {
                cancel(batches, batch.id());
            }
            throw exception;
        }
        return readResults(anthropicClient, batch.id(), calls.size());
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
        try {
            final var info = anthropicClient.models().retrieve(model);
            return new AiConnectionCheck(info.id(), info.displayName(), Duration.ofNanos(System.nanoTime() - start));
        } catch (RuntimeException exception) {
            throw toAiException(exception, model);
        }
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

    static MessageCreateParams toParams(AiCall call, String model, Collection<String> secrets) {
        final var builder = MessageCreateParams.builder()
            .model(model)
            .maxTokens(Optional.ofNullable(call.maxTokens()).orElse(DEFAULT_MAX_TOKENS))
            .addUserMessage(SecretMasker.maskText(call.prompt(), secrets))
            .putAdditionalHeader(BETA_HEADER, FALLBACK_BETA)
            .putAdditionalBodyProperty(FALLBACKS, JsonValue.from(FALLBACKS_DEFAULT));
        if (StringUtils.isNotBlank(call.system())) {
            builder.system(SecretMasker.maskText(call.system(), secrets));
        }
        toOutputConfig(call).ifPresent(builder::outputConfig);
        return builder.build();
    }

    /**
     * @return one request per call, each of them named after the index of its call. A batch takes no fallbacks.
     */
    static BatchCreateParams toBatchParams(List<AiCall> calls, String defaultModel, Collection<String> secrets) {
        final var builder = BatchCreateParams.builder();
        IntStream.range(0, calls.size()).forEach(index -> {
            final var call = calls.get(index);
            final var params = BatchCreateParams.Request.Params.builder()
                .model(Optional.ofNullable(call.model()).filter(StringUtils::isNotBlank).map(String::strip).orElse(defaultModel))
                .maxTokens(Optional.ofNullable(call.maxTokens()).orElse(DEFAULT_MAX_TOKENS))
                .addUserMessage(SecretMasker.maskText(call.prompt(), secrets));
            if (StringUtils.isNotBlank(call.system())) {
                params.system(SecretMasker.maskText(call.system(), secrets));
            }
            toOutputConfig(call).ifPresent(params::outputConfig);
            builder.addRequest(BatchCreateParams.Request.builder().customId(BATCH_ID_PREFIX + index).params(params.build()).build());
        });
        return builder.build();
    }

    static Optional<OutputConfig> toOutputConfig(AiCall call) {
        if (call.effort() == null && !call.isStructured()) {
            return Optional.empty();
        }
        final var builder = OutputConfig.builder();
        if (call.effort() != null) {
            builder.effort(OutputConfig.Effort.of(call.effort()));
        }
        if (call.isStructured()) {
            builder.format(toFormat(call.schema()));
        }
        return Optional.of(builder.build());
    }

    /**
     * @return the format which holds an answer to the schema. Anthropic takes an object only when it says that no
     *     other field may be added ({@code additionalProperties: false}), so every object of the schema which says
     *     nothing about it says that.
     */
    static JsonOutputFormat toFormat(Map<String, ?> schema) {
        final var schemaBuilder = JsonOutputFormat.Schema.builder();
        closeObjects(schema).forEach((key, value) -> schemaBuilder.putAdditionalProperty(key, JsonValue.from(value)));
        return JsonOutputFormat.builder().schema(schemaBuilder.build()).build();
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

    static AiBatchResult toBatchResult(MessageBatchResult result) {
        if (result.isSucceeded()) {
            try {
                return AiBatchResult.answered(toAnswer(result.asSucceeded().message()));
            } catch (AiException exception) {
                return AiBatchResult.failed(exception.getMessage());
            }
        }
        final var detail = result.isErrored()
            ? result.asErrored().error().error().message()
            : "the batch " + (result.isCanceled() ? "was cancelled" : "expired") + " before this call was answered.";
        return AiBatchResult.failed(new AiException(Reason.FAILED, detail).getMessage());
    }

    static AiAnswer toAnswer(Message message) {
        final var stopReason = message.stopReason().orElse(null);
        if (StopReason.REFUSAL.equals(stopReason)) {
            throw new AiException(Reason.REFUSED, message.stopDetails().map(AiClientHolder::describe).orElse(null));
        }
        final var text = message.content().stream()
            .flatMap(block -> block.text().stream())
            .map(TextBlock::text)
            .collect(Collectors.joining());
        return new AiAnswer(text, message.model().asString(), message.usage().inputTokens(), message.usage().outputTokens(),
            stopReason == null ? null : stopReason.asString());
    }

    static AiException toAiException(Throwable failure, String model) {
        var cause = failure;
        while ((cause instanceof CompletionException || cause instanceof ExecutionException) && cause.getCause() != null) {
            cause = cause.getCause();
        }
        return switch (cause) {
            case AiException aiException -> aiException;
            case UnauthorizedException exception -> new AiException(Reason.KEY_REJECTED, null, exception);
            case PermissionDeniedException exception -> new AiException(Reason.KEY_REJECTED, null, exception);
            case NotFoundException exception -> new AiException(Reason.MODEL_UNKNOWN, model, exception);
            case RateLimitException exception -> new AiException(Reason.RATE_LIMITED, null, exception);
            case AnthropicServiceException exception -> new AiException(Reason.FAILED, exception.getMessage(), exception);
            case AnthropicIoException exception -> new AiException(Reason.UNREACHABLE, null, exception);
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
     * @param wait how long to wait for this future.
     * @param timeout the timeout which the failure names: the one of the call, or of the whole batch.
     */
    private static <T> T await(CompletableFuture<T> future, Duration wait, Duration timeout, String model) {
        try {
            return future.get(wait.toMillis(), TimeUnit.MILLISECONDS);
        } catch (InterruptedException exception) {
            future.cancel(true);
            Thread.currentThread().interrupt();
            throw new AiException(Reason.CANCELLED, null, exception);
        } catch (TimeoutException exception) {
            future.cancel(true);
            throw new AiException(Reason.TIMEOUT, timeout.toString(), exception);
        } catch (ExecutionException exception) {
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

    private static void cancel(BatchServiceAsync batches, String batchId) {
        // what is left of the batch would be paid for, and nobody would read it
        batches.cancel(batchId).whenComplete((_, failure) -> {
            if (failure == null) {
                log.info("Batch '{}' of calls to Claude was cancelled.", batchId);
            } else {
                log.warn("Batch '{}' of calls to Claude could not be cancelled.", batchId, failure);
            }
        });
    }

    private static List<AiBatchResult> readResults(AnthropicClient anthropicClient, String batchId, int size) {
        final var results = new HashMap<String, MessageBatchResult>();
        try (var response = anthropicClient.messages().batches().resultsStreaming(batchId)) {
            response.stream().forEach(item -> results.put(item.customId(), item.result()));
        } catch (RuntimeException exception) {
            throw toAiException(exception, null);
        }
        return IntStream.range(0, size)
            .mapToObj(index -> Optional.ofNullable(results.get(BATCH_ID_PREFIX + index))
                .map(AiClientHolder::toBatchResult)
                .orElseGet(() -> AiBatchResult.failed(new AiException(Reason.FAILED, "the batch has no result for this call.").getMessage())))
            .toList();
    }

    private static boolean isFallbackRefused(AiException exception) {
        return exception.getReason() == Reason.FAILED && exception.getCause() instanceof BadRequestException
            && Strings.CI.contains(exception.getMessage(), "fallback");
    }

    private static MessageCreateParams withoutFallbacks(MessageCreateParams params) {
        return params.toBuilder()
            .removeAdditionalBodyProperty(FALLBACKS)
            .removeAdditionalHeaders(BETA_HEADER)
            .build();
    }

    private static String describe(RefusalStopDetails details) {
        final var parts = List.of(
            details.category().map(category -> "Category: " + category.asString() + ".").orElse(StringUtils.EMPTY),
            details.explanation().orElse(StringUtils.EMPTY));
        return parts.stream().filter(StringUtils::isNotBlank).collect(Collectors.joining(" "));
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

    private record Settings(BooleanSupplier enabled, Supplier<String> defaultModel, Supplier<? extends Collection<String>> secrets,
                            Supplier<Optional<AnthropicClient>> clientFactory) {

        static final Settings NONE = new Settings(() -> false, () -> DEFAULT_MODEL, List::of, Optional::empty);

    }

}
