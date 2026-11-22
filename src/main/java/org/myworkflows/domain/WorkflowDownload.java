package org.myworkflows.domain;

import java.time.Instant;
import java.util.UUID;

/**
 * A file which a run offers to download, in the directory of that run: {@code downloads/<run id>/<file name>}. Like a
 * {@link WorkflowReport}, it is known by what the file says when it is read, and nothing about it is kept anywhere else.
 *
 * @param size the size of the file in bytes.
 * @param created when the file was written.
 * @param zipEntries how many files the zip archive holds, or nothing when it is no zip archive.
 * @param zipContentSize how many bytes these files have once they are unpacked, or nothing when it is no zip archive.
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record WorkflowDownload(UUID workflowRunId, String fileName, long size, Instant created, Integer zipEntries, Long zipContentSize) {

    /**
     * @return what tells the file from any other: the id of its run and its name, {@code <run id>/<file name>}. It is
     *     what the download command outputs and what the REST API points to it by.
     */
    public String id() {
        return workflowRunId + "/" + fileName;
    }

    public boolean isZip() {
        return zipEntries != null;
    }

}
