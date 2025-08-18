package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.regex.Pattern;

import static java.util.regex.Matcher.quoteReplacement;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class StringReplacer {

    public static String replace(String input, Pattern regex, StringReplacerCallback callback) {
        final var builder = new StringBuilder();
        final var matcher = regex.matcher(input);
        while (matcher.find()) {
            matcher.appendReplacement(builder, quoteReplacement(callback.replace(matcher)));
        }
        matcher.appendTail(builder);
        return builder.toString();
    }

}
