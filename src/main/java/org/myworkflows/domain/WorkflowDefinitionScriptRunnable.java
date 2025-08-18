package org.myworkflows.domain;

import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;

import java.util.UUID;

import static org.myworkflows.domain.WorkflowDefinitionScript.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public record WorkflowDefinitionScriptRunnable(ApplicationManager applicationManager, WorkflowTemplate workflowTemplate) implements Runnable {

    @Override
    public void run() {
        applicationManager.getBeanOfType(EventBroadcaster.class)
            .broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                .token(UUID.randomUUID())
                .workflowRun(new WorkflowRun(workflowTemplate.getId(), workflowTemplate.getWorkflowDefinitionParameters()))
                .workflowDefinitionScript(of(workflowTemplate.getWorkflowDefinitionScripts()))
                .build());
    }

}
