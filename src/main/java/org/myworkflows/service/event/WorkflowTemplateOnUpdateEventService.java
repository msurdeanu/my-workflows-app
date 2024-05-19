package org.myworkflows.service.event;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowTemplateOnUpdateEvent;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class WorkflowTemplateOnUpdateEventService implements EventListener<WorkflowTemplateOnUpdateEvent> {

    private final ApplicationManager applicationManager;

    @Override
    public void onEventReceived(WorkflowTemplateOnUpdateEvent event) {
        applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(event.getWorkflowTemplate());
    }

    @Override
    public Class<WorkflowTemplateOnUpdateEvent> getEventType() {
        return WorkflowTemplateOnUpdateEvent.class;
    }

}
