package org.myworkflows.view.component;

import java.util.UUID;

/**
 * A page which shows how a run it started goes on, so that its user needs no notification once that run is over.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public interface WorkflowRunFollower {

    /**
     * Asked on the thread of the UI.
     *
     * @return whether the page shows the outcome of the run with the given id.
     */
    boolean follows(UUID workflowRunId);

}
