package org.myworkflows.domain.ai;

/**
 * Whether the application may call Claude right now.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public enum AiStatus {

    /**
     * An admin has not turned Claude on, on the features page.
     */
    DISABLED,

    /**
     * Claude is turned on, but the server has no API key: the {@code ANTHROPIC_API_KEY} environment variable is not set.
     */
    NO_KEY,

    /**
     * Claude is turned on and there is a key. Whether Anthropic takes it is known after a call, or a connection test.
     */
    READY

}
