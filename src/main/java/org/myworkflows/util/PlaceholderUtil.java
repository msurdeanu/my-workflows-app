package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PlaceholderUtil {

    public static final String PLACEHOLDER_NAME_PATTERN = "[A-Z0-9_.]+";

    public static final String ALLOWED_CHARS_FOR_PLACEHOLDER_NAME = "[A-Z0-9_.]";

    private static final Pattern PLACEHOLDER_PATTERN = Pattern.compile("\\$\\$\\((" + PLACEHOLDER_NAME_PATTERN + ")\\)");

    /**
     * @return the names of the placeholders the given text refers to, in the order it first names each of them.
     */
    public static Set<String> findPlaceholderNames(String value) {
        final var names = new LinkedHashSet<String>();
        if (value != null) {
            final var matcher = PLACEHOLDER_PATTERN.matcher(value);
            while (matcher.find()) {
                names.add(matcher.group(1));
            }
        }
        return names;
    }

    public static String resolvePlaceholders(String value, Map<String, String> placeholders) {
        return StringReplacer.replace(value, PLACEHOLDER_PATTERN, (Matcher matcher) -> {
            final var name = matcher.group(1);
            return ofNullable(placeholders.get(name)).orElseThrow(() -> new WorkflowRuntimeException("No placeholder found for name '" + name + "'"));
        });
    }

}
