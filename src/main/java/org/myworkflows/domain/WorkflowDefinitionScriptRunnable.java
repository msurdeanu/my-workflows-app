package org.myworkflows.domain;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.myworkflows.domain.WorkflowDefinitionScript.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowDefinitionScriptRunnable implements Runnable {

    private final ApplicationManager applicationManager;

    private final Map<String, Object> parameters;

    private final List<WorkflowDefinitionScript> workflowDefinitionScripts;

    @Override
    public void run() {
        // TODO: Inject parameters
        applicationManager.getBeanOfType(EventBroadcaster.class)
            .broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                .isManual(false)
                .token(UUID.randomUUID())
                .workflowDefinitionScript(of(workflowDefinitionScripts))
                .build());
    }

}
