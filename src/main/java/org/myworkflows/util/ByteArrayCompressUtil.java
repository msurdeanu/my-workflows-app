package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.io.ByteArrayOutputStream;
import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ByteArrayCompressUtil {

    public static byte[] compress(byte[] input) {
        final var deflater = new Deflater();
        deflater.setInput(input);
        deflater.finish();

        final var outputStream = new ByteArrayOutputStream();
        final var buffer = new byte[1024];
        while (!deflater.finished()) {
            final var compressedSize = deflater.deflate(buffer);
            outputStream.write(buffer, 0, compressedSize);
        }
        return outputStream.toByteArray();
    }

    public static byte[] decompress(byte[] input) throws DataFormatException {
        final var inflater = new Inflater();
        inflater.setInput(input);

        final var outputStream = new ByteArrayOutputStream();
        final var buffer = new byte[1024];
        while (!inflater.finished()) {
            final var decompressedSize = inflater.inflate(buffer);
            outputStream.write(buffer, 0, decompressedSize);
        }
        return outputStream.toByteArray();
    }

}
