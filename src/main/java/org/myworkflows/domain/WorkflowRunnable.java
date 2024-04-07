package org.myworkflows.domain;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;

import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowRunnable implements Runnable {

    private final ApplicationManager applicationManager;

    private final WorkflowDefinition workflowDefinition;

    @Override
    public void run() {
        applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                        .isManual(false)
                        .token(UUID.randomUUID())
                        .workflow(workflowDefinition)
                        .build());
    }

}
