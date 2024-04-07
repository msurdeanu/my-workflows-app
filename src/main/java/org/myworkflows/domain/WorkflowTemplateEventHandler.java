package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowTemplateEventHandler {

    void onDefinitionChanged(final WorkflowTemplate workflowTemplate, final String newDefinition);

}
