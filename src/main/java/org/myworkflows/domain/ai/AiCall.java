package org.myworkflows.domain.ai;

import lombok.Builder;
import org.jspecify.annotations.Nullable;

import java.time.Duration;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * One question for Claude, as a feature asks it: whatever is not given takes its default when it is sent (see
 * {@link org.myworkflows.holder.AiClientHolder#send(AiCall)}). The prompt and the system text are masked before they
 * leave the server.
 *
 * @param model the model to ask instead of the one of the settings.
 * @param maxTokens the most the answer may take, thinking included.
 * @param effort how hard Claude thinks: {@code low}, {@code medium}, {@code high}, {@code xhigh} or {@code max}.
 * @param schema a JSON schema, as a map, which the answer is held to: the answer is then a JSON text which
 *     {@link AiAnswer#toOutput(boolean)} reads. An empty schema is no schema.
 * @param timeout how long the call may take before it is given up. A call of a batch has none of its own.
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Builder
public record AiCall(String prompt, @Nullable String system, @Nullable String model, @Nullable Long maxTokens,
                     @Nullable String effort, @Nullable Map<String, ?> schema, @Nullable Duration timeout) {

    public static final Set<String> EFFORTS = Set.of("low", "medium", "high", "xhigh", "max");

    public AiCall {
        Objects.requireNonNull(prompt, "A call to Claude needs a prompt.");
        if (maxTokens != null && maxTokens < 1) {
            throw new IllegalArgumentException("The answer of Claude needs at least one token, not " + maxTokens + ".");
        }
        if (effort != null) {
            effort = effort.strip().toLowerCase(Locale.ROOT);
            if (!EFFORTS.contains(effort)) {
                throw new IllegalArgumentException("The effort of a call to Claude is one of " + EFFORTS + ", not '" + effort + "'.");
            }
        }
        if (schema != null && schema.isEmpty()) {
            schema = null;
        }
        if (timeout != null && (timeout.isZero() || timeout.isNegative())) {
            throw new IllegalArgumentException("A call to Claude needs some time, not " + timeout + ".");
        }
    }

    /**
     * @return whether the answer is held to a schema, and so is a JSON text.
     */
    public boolean isStructured() {
        return schema != null;
    }

}
