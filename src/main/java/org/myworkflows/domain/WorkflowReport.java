package org.myworkflows.domain;

import java.time.Instant;
import java.util.UUID;

/**
 * A file of the reports directory, in the directory of the run which wrote it: {@code reports/<run id>/<file name>}.
 * Like a {@link Library}, it is known by what the file says when it is read - its name, extension, size and date - and
 * nothing about it is kept anywhere else. Its title is its file name, without the extension.
 *
 * @param workflowTemplateId the template of the run, or nothing when the run had none or is not known anymore.
 * @param userId the user who started the run, or nothing when nobody did or the run is not known anymore.
 * @param size the size of the file in bytes.
 * @param created when the file was last written.
 * @author Mihai Surdeanu
 * @since 2.3
 */
public record WorkflowReport(UUID workflowRunId, String fileName, WorkflowReportFormat format, Integer workflowTemplateId, Integer userId,
                             long size, Instant created) {

    /**
     * @return what tells the report from any other: the id of its run and its file name, {@code <run id>/<file name>}.
     *     It is what the reports page and the REST API point to it by.
     */
    public String id() {
        return workflowRunId + "/" + fileName;
    }

    /**
     * @return the file name without its extension, which the command wrote as the title of the report.
     */
    public String title() {
        return fileName.substring(0, fileName.length() - format.getExtension().length());
    }

}
