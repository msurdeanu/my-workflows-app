package org.myworkflows.domain.ai;

import java.time.Duration;

/**
 * What a connection test found: Anthropic took the key of the server and knows the model of the settings. Asking
 * about a model uses no tokens.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record AiConnectionCheck(String model, String displayName, Duration took) {

}
