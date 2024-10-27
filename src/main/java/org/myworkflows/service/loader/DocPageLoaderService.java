package org.myworkflows.service.loader;

import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.repository.DocPageRepository;
import org.myworkflows.service.DocPageService;
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
public final class DocPageLoaderService implements LoaderService {

    private final ApplicationManager applicationManager;

    @Order(200)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        final var docPageService = applicationManager.getBeanOfType(DocPageService.class);
        applicationManager.getBeanOfType(DocPageRepository.class)
            .findAll()
            .forEach(docPageService::addToCache);
    }

}
