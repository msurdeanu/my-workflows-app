package org.myworkflows.service.event;

import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmittedEvent;
import org.myworkflows.service.WorkflowRunCacheService;
import org.springframework.stereotype.Service;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public final class WorkflowRunOnSubmittedEventService implements EventListener<WorkflowDefinitionOnSubmittedEvent> {

    private final WorkflowRunCacheService workflowRunCacheService;

    @Override
    public void onEventReceived(WorkflowDefinitionOnSubmittedEvent event) {
        ofNullable(event.getExecutionContext()).ifPresent(workflowRunCacheService::add);
    }

    @Override
    public Class<WorkflowDefinitionOnSubmittedEvent> getEventType() {
        return WorkflowDefinitionOnSubmittedEvent.class;
    }

}
