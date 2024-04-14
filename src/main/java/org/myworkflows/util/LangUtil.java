package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.Optional;

import static java.util.Optional.empty;
import static java.util.Optional.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class LangUtil {

    public static Optional<String> pluralize(final String word, final long count, final boolean inclusive) {
        if (word == null || count < 1) {
            return empty();
        }

        final var builder = new StringBuilder();
        if (inclusive) {
            builder.append(count).append(" ");
        }
        builder.append(word);
        if (count > 1) {
            builder.append("s");
        }
        return of(builder.toString());
    }

}
