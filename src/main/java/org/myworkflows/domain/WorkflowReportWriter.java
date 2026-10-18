package org.myworkflows.domain;

/**
 * Writes the report of a command. A command cannot reach the service which does it, so the run carries it.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@FunctionalInterface
public interface WorkflowReportWriter {

    /**
     * @return the report which was written, once its file is there.
     * @throws org.myworkflows.exception.WorkflowRuntimeException when the report cannot be written. Nothing of it is
     *     kept then.
     */
    WorkflowReport write(WorkflowRun workflowRun, String title, WorkflowReportFormat format, String content);

}
