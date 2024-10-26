package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class StringReplacerTest {

    private static final Pattern UPPERCASE_PATTERN = Pattern.compile("([A-Z]+)");

    @Test
    public void testUppercasePattern() {
        final var input = "bla bla BLA";
        assertEquals(input.toLowerCase(), StringReplacer.replace(input, UPPERCASE_PATTERN,
                (Matcher matcher) -> matcher.group(1).toLowerCase()));
    }

}
