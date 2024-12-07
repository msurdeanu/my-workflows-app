package org.myworkflows.service;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public class WorkflowRunProgressService implements EventListener<WorkflowDefinitionOnProgressEvent> {

    private final ApplicationManager applicationManager;

    @Transactional
    @Override
    public void onEventReceived(WorkflowDefinitionOnProgressEvent event) {
        final var service = applicationManager.getBeanOfType(WorkflowRunService.class);
        service.create(event.getWorkflowRun(), event.isPersisted());
        if (event.isPersisted()) {
            service.deleteOldEntriesIfNeeded(false);
        }
    }

    @Override
    public Class<WorkflowDefinitionOnProgressEvent> getEventType() {
        return WorkflowDefinitionOnProgressEvent.class;
    }

}
