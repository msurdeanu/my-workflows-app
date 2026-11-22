package org.myworkflows.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.jspecify.annotations.Nullable;

import java.util.Locale;

/**
 * Raised when Claude could not be asked, or did not answer. Its message is what the run of a command which called
 * Claude fails with; a page shows the {@link Reason#getTranslationKey() translation} of its reason instead.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Getter
public final class AiException extends RuntimeException {

    private final Reason reason;

    @Nullable
    private final String detail;

    public AiException(Reason reason) {
        this(reason, null, null);
    }

    public AiException(Reason reason, @Nullable String detail) {
        this(reason, detail, null);
    }

    public AiException(Reason reason, @Nullable String detail, @Nullable Throwable cause) {
        super(detail == null ? reason.getText() : reason.getText() + " " + detail, cause);
        this.reason = reason;
        this.detail = detail;
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.5
     */
    @Getter
    @RequiredArgsConstructor
    public enum Reason {
        DISABLED("Claude is turned off. An admin turns it on on the Features page."),
        NO_KEY("Claude is not configured on this server: the ANTHROPIC_API_KEY environment variable is not set."),
        KEY_REJECTED("Anthropic did not accept the API key of this server."),
        MODEL_UNKNOWN("Anthropic does not know this model:"),
        RATE_LIMITED("Anthropic asked to slow down: too many calls to Claude for now."),
        REFUSED("Claude declined to answer."),
        TRUNCATED("Claude reached the token limit of the call (maxTokens) before the end of its answer, after"),
        TIMEOUT("Claude did not answer in time:"),
        CANCELLED("The call to Claude was cancelled."),
        UNREACHABLE("Anthropic could not be reached."),
        FAILED("The call to Claude failed:");

        private final String text;

        public String getTranslationKey() {
            return "features.claude.error." + name().toLowerCase(Locale.ROOT).replace('_', '-');
        }
    }

}
