package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowDefinitionEventHandler {

    void onScriptChange(Integer workflowTemplateId, String newScript);

}
