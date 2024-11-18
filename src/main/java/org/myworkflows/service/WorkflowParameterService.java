package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.filter.WorkflowParameterFilter;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowParameterService extends CacheableDataService<WorkflowParameter, WorkflowParameterFilter> implements LoaderService {

    public WorkflowParameterService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_PARAMETER);
    }

    @Order(10)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        applicationManager.getBeanOfType(WorkflowParameterRepository.class)
            .findAll()
            .forEach(this::addToCache);
    }

    public void create(String name) {
        lock.lock();
        try {
            final var workflowParameter = WorkflowParameter.of(name);
            cache.put(applicationManager.getBeanOfType(WorkflowParameterRepository.class).save(workflowParameter).getName(),
                workflowParameter);
        } finally {
            lock.unlock();
        }
    }

    public void update(WorkflowParameter workflowParameter) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowParameterRepository.class).save(workflowParameter);
        } finally {
            lock.unlock();
        }
    }

    public void delete(WorkflowParameter workflowParameter) {
        lock.lock();
        try {
            cache.evict(workflowParameter.getName());
            applicationManager.getBeanOfType(WorkflowParameterRepository.class).delete(workflowParameter);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowParameterFilter createFilter() {
        return new WorkflowParameterFilter();
    }

}
