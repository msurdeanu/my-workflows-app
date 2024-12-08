package org.myworkflows.domain.handler;

import org.myworkflows.domain.WorkflowParameter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowParameterEventHandler {

    WorkflowParameter onCreate(String name);

    void onDelete(WorkflowParameter workflowParameter);

    void onUpdate(WorkflowParameter workflowParameter);

}
