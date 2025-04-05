package org.myworkflows.holder.file;

import lombok.Getter;
import lombok.Setter;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
public enum FileSourceHolder {
    INSTANCE;

    @Setter
    private String baseDirectory = "files/";

    public <T> T readFromSource(FileSource<T> fileSource) throws IOException {
        return fileSource.readFrom(getFileFullPath(fileSource));
    }

    public <T> void writeToSource(FileSource<T> fileSource, T data) throws IOException {
        fileSource.writeTo(getFileFullPath(fileSource), data);
    }

    public void deleteIfExists(FileSource<?> fileSource) throws IOException {
        Files.deleteIfExists(Paths.get(getFileFullPath(fileSource)));
    }

    private String getFileFullPath(FileSource<?> fileSource) {
        return baseDirectory + fileSource.getFileName() + fileSource.getFileExtension();
    }

}
