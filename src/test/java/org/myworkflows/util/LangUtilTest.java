package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
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

}
