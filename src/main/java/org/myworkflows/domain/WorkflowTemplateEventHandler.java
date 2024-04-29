package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowTemplateEventHandler {

    void onActivationChanged(final Integer workflowTemplateId);

    void onCronChanged(final Integer workflowTemplateId, final String newCron);

    void onDefinitionChanged(final Integer workflowTemplateId, final String newDefinition);

    void onDelete(final Integer workflowTemplateId);

    void onNameChanged(final Integer workflowTemplateId, final String newName);

    void onScheduleNow(final Integer workflowTemplateId);

}
