package org.myworkflows.view.component;

import org.myworkflows.domain.handler.WorkflowParameterEventHandler;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class WorkflowParameterGrid extends AbstractParameterGrid {

    public WorkflowParameterGrid(WorkflowParameterEventHandler workflowParameterEventHandler) {
        super("workflow-params");
        setCreateFunction(workflowParameterEventHandler::onCreate);
        setUpdateConsumer(workflowParameterEventHandler::onUpdate);
        setDeleteConsumer(workflowParameter -> new DeleteConfirmDialog(workflowParameter.getName(),
            item -> workflowParameterEventHandler.onDelete(workflowParameter)).open());
    }

}
