package org.myworkflows.domain.handler;

import org.myworkflows.domain.WorkflowDefinition;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowDefinitionEventHandler {

    void onCreate(String name);

    void onDelete(WorkflowDefinition workflowDefinition);

    void onUpdate(WorkflowDefinition workflowDefinition, String newName);

}
