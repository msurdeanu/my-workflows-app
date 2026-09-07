package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.io.ByteArrayOutputStream;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ByteArrayCompressUtil {

    private static final int BUFFER_SIZE = 1024;

    public static byte[] compress(byte[] input) {
        final var deflater = new Deflater();
        try {
            deflater.setInput(input);
            deflater.finish();

            final var outputStream = new ByteArrayOutputStream();
            final var buffer = new byte[BUFFER_SIZE];
            while (!deflater.finished()) {
                final var compressedSize = deflater.deflate(buffer);
                outputStream.write(buffer, 0, compressedSize);
            }
            return outputStream.toByteArray();
        } finally {
            // Deflater holds native memory that is only released on end().
            deflater.end();
        }
    }

    public static byte[] decompress(byte[] input) {
        final var inflater = new Inflater();
        try {
            inflater.setInput(input);

            return WorkflowRuntimeException.wrap(() -> {
                final var outputStream = new ByteArrayOutputStream();
                final var buffer = new byte[BUFFER_SIZE];
                while (!inflater.finished()) {
                    // Truncated input never reaches the finished state, so bail out instead of spinning forever.
                    if (inflater.needsInput() || inflater.needsDictionary()) {
                        throw new WorkflowRuntimeException("Compressed input is truncated or corrupted.");
                    }
                    final var decompressedSize = inflater.inflate(buffer);
                    outputStream.write(buffer, 0, decompressedSize);
                }
                return outputStream.toByteArray();
            });
        } finally {
            // Inflater holds native memory that is only released on end().
            inflater.end();
        }
    }

}
