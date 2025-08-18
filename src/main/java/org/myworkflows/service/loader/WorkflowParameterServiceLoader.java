package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.myworkflows.service.WorkflowParameterService;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
@RequiredArgsConstructor
public class WorkflowParameterServiceLoader implements ServiceLoader {

    private final ApplicationManager applicationManager;

    @Order(10)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        loadByServiceAndRepo(applicationManager, WorkflowParameterService.class, WorkflowParameterRepository.class);
    }

}
