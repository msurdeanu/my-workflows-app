package org.myworkflows.domain;

/**
 * Writes the file a command offers to download. A command cannot reach the service which does it, so the run carries
 * it.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@FunctionalInterface
public interface WorkflowDownloadWriter {

    /**
     * @return the file which was written, once it is there.
     * @throws org.myworkflows.exception.WorkflowRuntimeException when the file cannot be written. Nothing of it is kept
     *     then.
     */
    WorkflowDownload write(WorkflowRun workflowRun, WorkflowDownloadRequest request);

}
