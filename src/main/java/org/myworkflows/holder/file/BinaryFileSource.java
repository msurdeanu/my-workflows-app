package org.myworkflows.holder.file;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public record BinaryFileSource(String fileName, String fileExtension) implements FileSource<byte[]> {

    public static BinaryFileSource of(String fileName) {
        return new BinaryFileSource(fileName, ".bin");
    }

    @Override
    public byte[] readFrom(Path filePath) throws IOException {
        return Files.readAllBytes(filePath);
    }

    @Override
    public void writeTo(Path filePath, byte[] data) throws IOException {
        Files.write(filePath, data);
    }

}
