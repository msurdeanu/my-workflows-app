package org.myworkflows.service.loader;

import jakarta.transaction.Transactional;
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
public class WorkflowRunServiceLoader implements ServiceLoader {

    private final ApplicationManager applicationManager;
    private final CacheConfig cacheConfig;

    @Order(100)
    @Transactional
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var service = applicationManager.getBeanOfType(WorkflowRunService.class);
        service.deleteOldEntriesIfNeeded(true);
        applicationManager.getBeanOfType(WorkflowRunRepository.class)
            .findByOrderByCreatedDesc(PageRequest.of(0, cacheConfig.getWorkflowRunMaxSize()))
            .reversed()
            .forEach(item -> service.create(item, false));
    }

}
