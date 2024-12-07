package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.repository.WorkflowPlaceholderRepository;
import org.myworkflows.service.WorkflowPlaceholderService;
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
public class WorkflowPlaceholderServiceLoader implements ServiceLoader {

    private final ApplicationManager applicationManager;

    @Order(8)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        loadByServiceAndRepo(applicationManager, WorkflowPlaceholderService.class, WorkflowPlaceholderRepository.class);
    }

}
