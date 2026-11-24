package org.myworkflows.domain.ai;

import org.myworkflows.exception.AiException;
import org.myworkflows.exception.AiException.Reason;
import tools.jackson.core.JacksonException;
import tools.jackson.databind.json.JsonMapper;

/**
 * What Claude answered to an {@link AiCall}.
 *
 * @param text the text blocks of the answer, one after the other.
 * @param model the model which answered: another one than the one asked when it declined and a fallback answered.
 * @param stopReason why the answer ended, as the API says it: {@code end_turn}, or {@code max_tokens} when the answer
 *     was cut at the limit of the call.
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record AiAnswer(String text, String model, long inputTokens, long outputTokens, String stopReason) {

    public static final String MAX_TOKENS = "max_tokens";

    private static final JsonMapper OBJECT_MAPPER = JsonMapper.builder().build();

    /**
     * @return whether the answer was cut because it reached the most tokens the call allowed.
     */
    public boolean isTruncated() {
        return MAX_TOKENS.equals(stopReason);
    }

    /**
     * @param structured whether the call held the answer to a schema.
     * @return what a command outputs: the text, or - for an answer held to a schema - what its JSON says, a
     *     {@code Map} for an object, which keeps the order of its fields.
     * @throws AiException when the answer was cut at the limit of the call, which leaves a text half written and a JSON
     *     which cannot be read, or when the JSON cannot be read anyway.
     */
    public Object toOutput(boolean structured) {
        if (isTruncated()) {
            throw new AiException(Reason.TRUNCATED, outputTokens + " tokens");
        }
        if (!structured) {
            return text;
        }
        try {
            return OBJECT_MAPPER.readValue(text, Object.class);
        } catch (JacksonException exception) {
            throw new AiException(Reason.FAILED, "the answer is not the JSON its schema asks for.", exception);
        }
    }

}
