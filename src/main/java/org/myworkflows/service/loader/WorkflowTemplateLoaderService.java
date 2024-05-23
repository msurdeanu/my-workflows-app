package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.myworkflows.service.WorkflowTemplateService;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public final class WorkflowTemplateLoaderService implements LoaderService {

    private final ApplicationManager applicationManager;

    @Order(5)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var workflowTemplateService = applicationManager.getBeanOfType(WorkflowTemplateService.class);
        applicationManager.getBeanOfType(WorkflowTemplateRepository.class)
            .findAll()
            .forEach(workflowTemplateService::loadAndSchedule);
    }

}
