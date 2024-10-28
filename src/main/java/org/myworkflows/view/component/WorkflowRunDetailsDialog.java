package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import org.myworkflows.domain.WorkflowRun;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunDetailsDialog extends ResponsiveDialog {

    public WorkflowRunDetailsDialog(WorkflowRun workflowRun) {
        super("workflow-run-details");

        add(createBody(workflowRun));
    }

    private Component createBody(WorkflowRun workflowRun) {
        return new WorkflowPrintGrid(workflowRun.getAllPrints());
    }

}
