package org.myworkflows.service.event;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnDeleteEvent;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class WorkflowDefinitionOnDeleteEventService implements EventListener<WorkflowDefinitionOnDeleteEvent> {

    private final ApplicationManager applicationManager;

    @Override
    public void onEventReceived(WorkflowDefinitionOnDeleteEvent event) {
        applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).delete(event.getWorkflowDefinition());
    }

    @Override
    public Class<WorkflowDefinitionOnDeleteEvent> getEventType() {
        return WorkflowDefinitionOnDeleteEvent.class;
    }

}
