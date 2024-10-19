package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowDefinitionEventHandler {

    void onDelete(WorkflowDefinition workflowDefinition);

    void onNameUpdated(WorkflowDefinition workflowDefinition, String newName);

}
