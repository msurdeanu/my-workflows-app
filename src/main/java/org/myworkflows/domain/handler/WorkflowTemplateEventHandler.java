package org.myworkflows.domain.handler;

import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0
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
