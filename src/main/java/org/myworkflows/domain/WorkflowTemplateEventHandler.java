package org.myworkflows.domain;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface WorkflowTemplateEventHandler {

    void onActivationChanged(WorkflowTemplate workflowTemplate);

    void onCreate(String name);

    void onDefinitionUpdated(WorkflowTemplate workflowTemplate, Stream<WorkflowDefinition> items);

    void onParameterUpdated(WorkflowTemplate workflowTemplate, Stream<WorkflowParameter> items);

    void onDelete(WorkflowTemplate workflowTemplate);

    void onNameAndCronUpdated(WorkflowTemplate workflowTemplate, String newName, String newCron);

    void onScheduleNow(WorkflowTemplate workflowTemplate);

}
