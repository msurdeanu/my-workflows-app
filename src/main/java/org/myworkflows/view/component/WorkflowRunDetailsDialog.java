package org.myworkflows.view.component;

import org.myworkflows.domain.WorkflowRun;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class WorkflowRunDetailsDialog extends ResponsiveDialog {

    public WorkflowRunDetailsDialog(WorkflowRun workflowRun) {
        super("workflow-run-details");

        add(new WorkflowPrintGrid(workflowRun.getAllPrints()));
    }

}
