package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Optional;

import static java.lang.String.join;
import static java.util.Optional.empty;
import static java.util.Optional.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class LangUtil {

    private static final int SIZE_UNIT = 1024;

    private static final String[] SIZE_UNITS = {"KB", "MB", "GB", "TB"};

    public static Optional<String> pluralize(String word, long count) {
        return pluralize(word, count, true);
    }

    public static Optional<String> pluralize(String word, long count, boolean inclusive) {
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

    /**
     * @return the given duration in hours, minutes and seconds, like "1 hr and 5 mins". Whatever is zero is left out.
     */
    public static String describe(Duration duration) {
        final var parts = new ArrayList<String>(3);
        var remainingDuration = duration;
        pluralize("hr", remainingDuration.toHours()).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusHours(remainingDuration.toHours());
        pluralize("min", remainingDuration.toMinutes()).ifPresent(parts::add);
        remainingDuration = remainingDuration.minusMinutes(remainingDuration.toMinutes());
        pluralize("sec", remainingDuration.toSeconds()).ifPresent(parts::add);

        return join(" and ", parts);
    }

    /**
     * @return the given number of bytes in the largest unit which keeps it at one or more, like "1.5 MB". A unit is
     *     1024 times the one before it.
     */
    public static String describeSize(long bytes) {
        if (bytes < SIZE_UNIT) {
            return bytes + " B";
        }
        var size = (double) bytes / SIZE_UNIT;
        var unit = 0;
        while (size >= SIZE_UNIT && unit < SIZE_UNITS.length - 1) {
            size /= SIZE_UNIT;
            unit++;
        }
        return String.format(Locale.ROOT, "%.1f %s", size, SIZE_UNITS[unit]);
    }

}
