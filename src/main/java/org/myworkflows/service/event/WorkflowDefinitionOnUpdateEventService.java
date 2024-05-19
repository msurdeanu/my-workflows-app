package org.myworkflows.service.event;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnUpdateEvent;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class WorkflowDefinitionOnUpdateEventService implements EventListener<WorkflowDefinitionOnUpdateEvent> {

    private final ApplicationManager applicationManager;

    @Override
    public void onEventReceived(WorkflowDefinitionOnUpdateEvent event) {
        applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(event.getWorkflowDefinition());
    }

    @Override
    public Class<WorkflowDefinitionOnUpdateEvent> getEventType() {
        return WorkflowDefinitionOnUpdateEvent.class;
    }

}
