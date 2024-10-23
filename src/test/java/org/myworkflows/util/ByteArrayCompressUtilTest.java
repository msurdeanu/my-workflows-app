package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import java.util.zip.DataFormatException;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ByteArrayCompressUtilTest {

    @Test
    public void whenCompressionAndDecompressionIsUsedEverythingWorksAsExpected() throws DataFormatException {
        // given
        final var myString = "test_test_test";

        // when & then
        assertEquals(myString, new String(ByteArrayCompressUtil.decompress(ByteArrayCompressUtil.compress(myString.getBytes()))));
    }

}
