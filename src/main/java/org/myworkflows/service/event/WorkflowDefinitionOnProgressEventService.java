package org.myworkflows.service.event;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.repository.WorkflowRunRepository;
import org.myworkflows.service.WorkflowRunService;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public class WorkflowDefinitionOnProgressEventService implements EventListener<WorkflowDefinitionOnProgressEvent> {

    private final ApplicationManager applicationManager;

    @Override
    public void onEventReceived(WorkflowDefinitionOnProgressEvent event) {
        final var workflowRun = event.getWorkflowRun();

        if (event.isPersisted()) {
            applicationManager.getBeanOfType(WorkflowRunRepository.class).save(workflowRun);
            applicationManager.getBeanOfType(WorkflowRunService.class).addToCache(workflowRun);
        }
    }

    @Override
    public Class<WorkflowDefinitionOnProgressEvent> getEventType() {
        return WorkflowDefinitionOnProgressEvent.class;
    }

}
