package org.myworkflows.util;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

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

    @Test
    @Timeout(10)
    public void whenInputIsTruncatedThenAnExceptionIsThrownInsteadOfLoopingForever() {
        // given
        final var compressed = ByteArrayCompressUtil.compress("test_test_test".repeat(100).getBytes());
        final var truncated = Arrays.copyOf(compressed, compressed.length / 2);

        // when and then
        assertThrows(WorkflowRuntimeException.class, () -> ByteArrayCompressUtil.decompress(truncated));
    }

    @Test
    @Timeout(10)
    public void whenInputIsEmptyThenAnExceptionIsThrownInsteadOfLoopingForever() {
        // when and then
        assertThrows(WorkflowRuntimeException.class, () -> ByteArrayCompressUtil.decompress(new byte[0]));
    }

}
