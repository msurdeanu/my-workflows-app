package org.myworkflows.holder.file;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public final class BinaryFileSource implements FileSource<byte[]> {

    @Getter
    private final String fileName;

    @Getter
    private final String fileExtension;

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
