package org.myworkflows.holder.file;

import java.io.IOException;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface FileSource<T> {

    String getFileName();

    String getFileExtension();

    T readFrom(String fileFullPath) throws IOException;

    void writeTo(String fileFullPath, T data) throws IOException;

}
