package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import static com.networknt.schema.utils.StringUtils.isBlank;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Base64.getEncoder;
import static org.myworkflows.util.ByteArrayCompressUtil.compress;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class Base64Util {

    private static final int COMPRESSION_THRESHOLD = 100;
    private static final String COMPRESSION_FLAG = "_";

    public static String base64Encode(String value) {
        if (isBlank(value)) {
            return value;
        }

        final var valueAsBytes = value.getBytes(UTF_8);
        if (valueAsBytes.length >= COMPRESSION_THRESHOLD) {
            return COMPRESSION_FLAG + getEncoder().encodeToString(compress(valueAsBytes));
        } else {
            return getEncoder().encodeToString(valueAsBytes);
        }
    }

    public static String base64Decode(String value) {
        if (isBlank(value)) {
            return value;
        }

        try {
            String newValue = value;
            boolean isCompressed = value.startsWith(COMPRESSION_FLAG);
            if (isCompressed) {
                newValue = value.substring(1);
            }
            final var decode = Base64.getDecoder().decode(newValue.getBytes(UTF_8));
            return new String(isCompressed ? ByteArrayCompressUtil.decompress(decode) : decode, StandardCharsets.UTF_8);
        } catch (Exception notUsed) {
            return StringUtils.EMPTY;
        }
    }

}
