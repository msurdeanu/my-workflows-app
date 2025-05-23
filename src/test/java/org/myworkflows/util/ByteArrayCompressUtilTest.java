package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ByteArrayCompressUtilTest {

    @Test
    public void whenCompressionAndDecompressionIsUsedEverythingWorksAsExpected() {
        // given
        final var myString = "test_test_test";

        // when and then
        assertEquals(myString, new String(ByteArrayCompressUtil.decompress(ByteArrayCompressUtil.compress(myString.getBytes()))));
    }

}
