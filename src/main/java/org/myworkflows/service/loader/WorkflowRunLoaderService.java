package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.config.CacheConfig;
import org.myworkflows.repository.WorkflowRunRepository;
import org.myworkflows.service.WorkflowRunService;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public final class WorkflowRunLoaderService implements LoaderService {

    private final ApplicationManager applicationManager;

    @Order(100)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var cacheConfig = applicationManager.getBeanOfType(CacheConfig.class);
        final var workflowRunService = applicationManager.getBeanOfType(WorkflowRunService.class);
        applicationManager.getBeanOfType(WorkflowRunRepository.class)
                .findByOrderByCreatedDesc(PageRequest.of(0, cacheConfig.getWorkflowRunMaxSize()))
                .forEach(workflowRunService::addToCacheAtTheEnd);
    }

}
