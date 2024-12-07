package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.filter.WorkflowPlaceholderFilter;
import org.myworkflows.repository.WorkflowPlaceholderRepository;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowPlaceholderService extends CacheableDataService<WorkflowPlaceholder, WorkflowPlaceholderFilter>
    implements ServiceCreator<WorkflowPlaceholder> {

    public WorkflowPlaceholderService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_PLACEHOLDER);
    }

    public Map<String, String> getAllAsMap() {
        return cache.getAllValues().stream()
            .filter(item -> item instanceof WorkflowPlaceholder)
            .map(item -> (WorkflowPlaceholder) item)
            .collect(Collectors.toMap(WorkflowPlaceholder::getName, WorkflowPlaceholder::getValue));
    }

    @Override
    @CachePut(cacheNames = "workflowPlaceholder", key = "#result.name")
    public WorkflowPlaceholder create(WorkflowPlaceholder workflowPlaceholder, boolean requiresPersistence) {
        if (requiresPersistence) {
            lock.lock();
            try {
                applicationManager.getBeanOfType(WorkflowPlaceholderRepository.class).save(workflowPlaceholder);
            } finally {
                lock.unlock();
            }
        }

        return workflowPlaceholder;
    }

    public void update(WorkflowPlaceholder workflowPlaceholder) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowPlaceholderRepository.class).save(workflowPlaceholder);
        } finally {
            lock.unlock();
        }
    }

    @CacheEvict(cacheNames = "workflowPlaceholder", key = "#workflowPlaceholder.name")
    public void delete(WorkflowPlaceholder workflowPlaceholder) {
        lock.lock();
        try {
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
