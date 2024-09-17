package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.myworkflows.service.WorkflowDefinitionService;
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
public final class WorkflowDefinitionLoaderService implements LoaderService {

    private final ApplicationManager applicationManager;

    @Order(10)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var workflowDefinitionService = applicationManager.getBeanOfType(WorkflowDefinitionService.class);
        applicationManager.getBeanOfType(WorkflowDefinitionRepository.class)
            .findAll()
            .forEach(workflowDefinitionService::addToCache);
    }

}
