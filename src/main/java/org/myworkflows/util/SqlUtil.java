package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class SqlUtil {

    public static String escape(String value) {
        // escape single quotes by replacing them with two single quotes
        return ofNullable(value).map(it -> it.replace("'", "''")).orElse(null);
    }

}
