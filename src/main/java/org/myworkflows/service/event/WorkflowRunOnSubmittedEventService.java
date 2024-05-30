package org.myworkflows.service.event;

import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.service.WorkflowRunService;
import org.springframework.stereotype.Service;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public final class WorkflowRunOnSubmittedEventService implements EventListener<WorkflowDefinitionOnSubmittedEvent> {

    private final WorkflowRunService workflowRunService;

    @Override
    public void onEventReceived(WorkflowDefinitionOnSubmittedEvent event) {
        ofNullable(event.getExecutionContext()).ifPresent(workflowRunService::addToCache);
    }

    @Override
    public Class<WorkflowDefinitionOnSubmittedEvent> getEventType() {
        return WorkflowDefinitionOnSubmittedEvent.class;
    }

}
