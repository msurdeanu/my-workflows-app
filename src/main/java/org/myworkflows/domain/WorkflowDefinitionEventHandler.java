package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowDefinitionEventHandler {

    void onDelete(Integer workflowDefinitionId);

    void onNameUpdated(Integer workflowDefinitionId, String newName);

    void onScriptUpdated(Integer workflowDefinitionId, String newScript);

}
