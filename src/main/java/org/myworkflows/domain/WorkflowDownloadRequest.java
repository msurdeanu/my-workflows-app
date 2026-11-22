package org.myworkflows.domain;

import org.myworkflows.domain.download.DownloadSource;

/**
 * What a download command asks for: a file or a directory to offer, under which name and how compressed.
 *
 * @param fileName the name of the file to offer, or nothing to keep the one of the source.
 * @param compressAbove the size in bytes above which {@link WorkflowDownloadCompression#AUTO} zips a file.
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record WorkflowDownloadRequest(DownloadSource source, String fileName, WorkflowDownloadCompression compression, long compressAbove) {

}
