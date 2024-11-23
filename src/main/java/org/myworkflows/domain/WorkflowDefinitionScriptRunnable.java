package org.myworkflows.domain;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;

import java.util.UUID;

import static org.myworkflows.domain.WorkflowDefinitionScript.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowDefinitionScriptRunnable implements Runnable {

    private final ApplicationManager applicationManager;

    private final WorkflowTemplate workflowTemplate;

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
