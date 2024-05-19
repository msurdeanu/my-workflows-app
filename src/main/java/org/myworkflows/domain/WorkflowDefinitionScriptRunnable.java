package org.myworkflows.domain;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;

import java.util.List;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowDefinitionScriptRunnable implements Runnable {

    private final ApplicationManager applicationManager;

    private final List<WorkflowDefinitionScript> workflowDefinitionScripts;

    @Override
    public void run() {
        applicationManager.getBeanOfType(EventBroadcaster.class)
            .broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                .isManual(false)
                .token(UUID.randomUUID())
                .workflowDefinitionScript(WorkflowDefinitionScript.of(workflowDefinitionScripts))
                .build());
    }

}
