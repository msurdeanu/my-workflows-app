package org.myworkflows.view.component;

import org.myworkflows.domain.handler.WorkflowParameterEventHandler;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowParameterGrid extends AbstractParameterGrid {

    public WorkflowParameterGrid(WorkflowParameterEventHandler workflowParameterEventHandler) {
        super("workflow-params");
        setCreateConsumer(workflowParameterEventHandler::onCreate);
        setUpdateConsumer(workflowParameterEventHandler::onUpdate);
        setDeleteConsumer(workflowParameterEventHandler::onDelete);
    }

}
