package org.myworkflows.util;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class Base64UtilTest {

    @Test
    public void whenNoCompressionAndDecompressionIsUsedEverythingWorksAsExpected() {
        // given
        final var myString = "test";

        // when and then
        final var encodedValue = Base64Util.base64Encode(myString);
        assertEquals("dGVzdA==", encodedValue);
        assertEquals(myString, Base64Util.base64Decode(encodedValue));
    }

    @Test
    public void whenCompressionAndDecompressionIsUsedEverythingWorksAsExpected() {
        // given
        final var myString = "1234567890";
        final var stringBuilder = new StringBuilder();
        IntStream.range(0, 10).forEach(iteration -> stringBuilder.append(myString));

        // when and then
        final var encodedValue = Base64Util.base64Encode(stringBuilder.toString());
        assertEquals("_eJwzNDI2MTUzt7A0MKQZCwAKzhSD", encodedValue);
        assertTrue(encodedValue.length() < stringBuilder.length());
        assertEquals(stringBuilder.toString(), Base64Util.base64Decode(encodedValue));
    }

    @Test
    public void whenDecompressionIsUsedButEncodedValueIsMalformedNoExceptionIsThrown() {
        // given
        final var myEncodedString = "_aJwzNDI2MTUzt7A0MKQZCwAKzhSD";

        // when and then
        assertEquals(StringUtils.EMPTY, Base64Util.base64Decode(myEncodedString));
    }

}
