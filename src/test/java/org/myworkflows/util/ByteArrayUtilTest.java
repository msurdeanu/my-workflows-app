package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ByteArrayUtilTest {

    @Test
    public void whenWePlayWithAStringEverythingWorksAsExpected() {
        // given
        final var myString = "test";

        // when and then
        assertEquals(myString, new String(ByteArrayUtil.toPrimitive(ByteArrayUtil.toObject(myString.getBytes()))));
    }

}
