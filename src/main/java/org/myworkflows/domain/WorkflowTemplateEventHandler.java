package org.myworkflows.domain;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowTemplateEventHandler {

    void onActivationChanged(Integer workflowTemplateId);

    void onDefinitionUpdated(Integer workflowTemplateId, Stream<WorkflowDefinition> items);

    void onDelete(Integer workflowTemplateId);

    void onRename(Integer workflowTemplateId, String newName);

    void onReschedule(Integer workflowTemplateId, String newCron);

    void onScheduleNow(Integer workflowTemplateId);

}
