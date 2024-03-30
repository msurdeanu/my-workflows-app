package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PlaceholderUtil {

    private static final Pattern PLACEHOLDER_PATTERN = Pattern.compile("\\$\\(([a-zA-Z0-9_]+)\\)");

    public static String resolvePlaceholders(final String value, final Map<String, String> placeholders) {
        return StringReplacer.replace(value, PLACEHOLDER_PATTERN, (Matcher matcher) -> {
            final var name = matcher.group(1);
            return ofNullable(placeholders.get(name))
                    .orElseThrow(() -> new WorkflowRuntimeException("No placeholder found for name: " + name));
        });
    }
}
