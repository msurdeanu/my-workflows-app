package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowParameterEventHandler {

    void onCreate(String name);

    void onDelete(WorkflowParameter workflowParameter);

    void onUpdate(WorkflowParameter workflowParameter);

}