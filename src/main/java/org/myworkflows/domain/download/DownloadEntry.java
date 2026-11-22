package org.myworkflows.domain.download;

/**
 * A file which a download reads.
 *
 * @param location where the reader finds it.
 * @param path its name in a zip archive: the name of the directory which is downloaded, followed by its path in there,
 *     with {@code /} between the names.
 * @param size its size in bytes, or {@code -1} when it is not known before it is read.
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record DownloadEntry(String location, String path, long size) {

    public static final long UNKNOWN_SIZE = -1;

}
