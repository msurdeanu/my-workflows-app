package org.myworkflows.domain.download;

import java.io.IOException;

/**
 * Where a download command finds what it offers: a text of the run, a file or a directory of this server, or one of
 * another server, read over SSH.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public sealed interface DownloadSource permits ContentDownloadSource, LocalDownloadSource, RemoteDownloadSource {

    /**
     * @return a reader of the source, which the caller closes.
     * @throws IOException when there is nothing to read there, or it cannot be reached.
     */
    DownloadReader open() throws IOException;

}
