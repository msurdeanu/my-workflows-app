package org.myworkflows.holder.file;

import lombok.Getter;
import lombok.Setter;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static java.nio.file.Paths.get;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public enum FileSourceHolder {
    INSTANCE;

    @Setter
    private String baseDirectory = "files";

    public <T> T readFromSource(FileSource<T> fileSource) throws IOException {
        return fileSource.readFrom(getFilePath(fileSource));
    }

    public <T> void writeToSource(FileSource<T> fileSource, T data) throws IOException {
        fileSource.writeTo(getFilePath(fileSource), data);
    }

    public void deleteIfExists(FileSource<?> fileSource) throws IOException {
        Files.deleteIfExists(getFilePath(fileSource));
    }

    private Path getFilePath(FileSource<?> fileSource) {
        return get(baseDirectory + File.separator + fileSource.getFileName() + fileSource.getFileExtension());
    }

}
