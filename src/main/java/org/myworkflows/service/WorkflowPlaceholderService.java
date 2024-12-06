package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.filter.WorkflowPlaceholderFilter;
import org.myworkflows.repository.WorkflowPlaceholderRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowPlaceholderService extends CacheableDataService<WorkflowPlaceholder, WorkflowPlaceholderFilter> implements LoaderService {

    public WorkflowPlaceholderService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_PLACEHOLDER);
    }

    @Order(10)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        applicationManager.getBeanOfType(WorkflowPlaceholderRepository.class)
            .findAll()
            .forEach(this::addToCache);
    }

    public Map<String, String> getAllAsMap() {
        return cache.getAllValues().stream()
            .filter(item -> item instanceof WorkflowPlaceholder)
            .map(item -> (WorkflowPlaceholder) item)
            .collect(Collectors.toMap(WorkflowPlaceholder::getName, WorkflowPlaceholder::getValue));
    }

    public void create(String name) {
        lock.lock();
        try {
            final var workflowPlaceholder = WorkflowPlaceholder.of(name);
            cache.put(applicationManager.getBeanOfType(WorkflowPlaceholderRepository.class).save(workflowPlaceholder).getName(),
                workflowPlaceholder);
        } finally {
            lock.unlock();
        }
    }

    public void update(WorkflowPlaceholder workflowPlaceholder) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowPlaceholderRepository.class).save(workflowPlaceholder);
        } finally {
            lock.unlock();
        }
    }

    public void delete(WorkflowPlaceholder workflowPlaceholder) {
        lock.lock();
        try {
            cache.evict(workflowPlaceholder.getName());
            applicationManager.getBeanOfType(WorkflowPlaceholderRepository.class).delete(workflowPlaceholder);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowPlaceholderFilter createFilter() {
        return new WorkflowPlaceholderFilter();
    }

}
