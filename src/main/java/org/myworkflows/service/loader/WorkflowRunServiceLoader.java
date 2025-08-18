package org.myworkflows.service.loader;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.WorkflowRunRepository;
import org.myworkflows.service.WorkflowRunService;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
@RequiredArgsConstructor
public class WorkflowRunServiceLoader implements ServiceLoader {

    private final ApplicationManager applicationManager;

    @Order(100)
    @Transactional
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var service = applicationManager.getBeanOfType(WorkflowRunService.class);
        service.deleteOldEntriesIfNeeded(true);
        final var workflowRunMaxSize = applicationManager.getBeanOfType(SettingProvider.class).getOrDefault("workflowRunMaxSize", 250);
        applicationManager.getBeanOfType(WorkflowRunRepository.class)
            .findByOrderByCreatedDesc(PageRequest.of(0, workflowRunMaxSize))
            .reversed()
            .forEach(item -> service.create(item, false));
    }

}
