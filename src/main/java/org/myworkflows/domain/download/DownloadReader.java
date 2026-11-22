package org.myworkflows.domain.download;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

/**
 * Reads a source which was opened: the one file it is, or the files of the directory it is.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public interface DownloadReader extends Closeable {

    /**
     * @return the name of the file or of the directory, without the directories it is in.
     */
    String name();

    boolean isDirectory();

    /**
     * @return the one entry of a file, or the files of a directory and of the directories in it, the links left out.
     */
    List<DownloadEntry> entries() throws IOException;

    /**
     * Writes what the entry holds to the stream, which it neither flushes nor closes.
     */
    void copy(DownloadEntry entry, OutputStream outputStream) throws IOException;

    @Override
    default void close() throws IOException {
        // nothing is held open by default
    }

}
