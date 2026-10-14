package org.myworkflows.domain.event;

import org.myworkflows.domain.WorkflowRun;

/**
 * Published once a workflow run was written to the cache - and to the database, when it is completed. Whoever lists
 * the runs from there, like the runs page, is certain to find the run as it is now.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowRunOnChangeEvent(WorkflowRun workflowRun, boolean completed) implements Event {

}
