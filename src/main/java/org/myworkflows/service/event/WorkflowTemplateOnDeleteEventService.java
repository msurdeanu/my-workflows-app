package org.myworkflows.service.event;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowTemplateOnDeleteEvent;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class WorkflowTemplateOnDeleteEventService implements EventListener<WorkflowTemplateOnDeleteEvent> {

    private final ApplicationManager applicationManager;

    @Override
    public void onEventReceived(final WorkflowTemplateOnDeleteEvent event) {
        applicationManager.getBeanOfType(WorkflowTemplateRepository.class)
                .delete(event.getWorkflowTemplate());
    }

    @Override
    public Class<WorkflowTemplateOnDeleteEvent> getEventType() {
        return WorkflowTemplateOnDeleteEvent.class;
    }

}
