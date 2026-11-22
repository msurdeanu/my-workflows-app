package org.myworkflows.domain.event;

import org.myworkflows.domain.WorkflowDownload;

/**
 * Published once a run wrote a file to download, which is there by then: whoever follows the run can offer it.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record WorkflowDownloadOnWriteEvent(WorkflowDownload workflowDownload) implements Event {

}
