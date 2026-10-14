package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.myworkflows.util.LangUtil.describeSize;
import static org.myworkflows.util.LangUtil.pluralize;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class LangUtilTest {

    @Test
    public void pluralizeWorksAsExpected() {
        assertTrue(pluralize(null, 10, false).isEmpty());
        assertTrue(pluralize("word", 0, false).isEmpty());
        assertTrue(pluralize("word", 0, true).isEmpty());
        var pluralized = pluralize("word", 10, false);
        assertTrue(pluralized.isPresent());
        assertEquals("words", pluralized.get());
        pluralized = pluralize("word", 10, true);
        assertTrue(pluralized.isPresent());
        assertEquals("10 words", pluralized.get());
    }

    @Test
    public void describeSizeUsesTheLargestUnitWhichFits() {
        assertEquals("0 B", describeSize(0));
        assertEquals("1023 B", describeSize(1023));
        assertEquals("1.0 KB", describeSize(1024));
        assertEquals("1.5 KB", describeSize(1536));
        assertEquals("1.0 MB", describeSize(1024 * 1024));
        assertEquals("128.0 MB", describeSize(128L * 1024 * 1024));
        assertEquals("2.5 GB", describeSize(5L * 1024 * 1024 * 1024 / 2));
        // the last unit takes whatever is larger than itself
        assertEquals("2048.0 TB", describeSize(2048L * 1024 * 1024 * 1024 * 1024));
    }

}
