package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowTemplateEventHandler {

    void onActivationChanged(final Integer workflowTemplateId);

    void onDefinitionChanged(final Integer workflowTemplateId, final String newDefinition);

    void onDelete(final Integer workflowTemplateId);

    void onScheduleNow(final Integer workflowTemplateId);

}
