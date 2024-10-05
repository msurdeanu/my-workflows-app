package org.myworkflows.domain;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowDefinitionEventHandler {

    void onDelete(WorkflowDefinition workflowDefinition);

    void onNameUpdated(WorkflowDefinition workflowDefinition, String newName);

    void onParameterUpdated(WorkflowDefinition workflowDefinition, Stream<WorkflowParameter> items);

    void onScriptUpdated(WorkflowDefinition workflowDefinition, String newScript);

}
