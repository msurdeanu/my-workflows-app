package org.myworkflows.domain;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowDefinitionEventHandler {

    void onDelete(Integer workflowDefinitionId);

    void onNameUpdated(Integer workflowDefinitionId, String newName);

    void onParameterUpdated(Integer workflowDefinitionId, Stream<WorkflowParameter> items);

    void onScriptUpdated(Integer workflowDefinitionId, String newScript);

}
