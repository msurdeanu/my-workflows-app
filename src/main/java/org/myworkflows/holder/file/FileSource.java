package org.myworkflows.holder.file;

import java.io.IOException;
import java.nio.file.Path;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface FileSource<T> {

    String getFileName();

    String getFileExtension();

    T readFrom(Path filePath) throws IOException;

    void writeTo(Path filePath, T data) throws IOException;

}
