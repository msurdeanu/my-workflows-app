package org.myworkflows.service;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.config.CacheConfig;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.repository.WorkflowRunRepository;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static java.util.Optional.ofNullable;
import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_RUN_NAME;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public class WorkflowRunService extends CacheableDataService<WorkflowRun, WorkflowRunFilter> implements ServiceCreator<WorkflowRun> {

    private final CacheConfig cacheConfig;
    private final InternalCache templateCache;

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_RUN);
        cacheConfig = applicationManager.getBeanOfType(CacheConfig.class);
        templateCache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName());
    }

    public Optional<WorkflowTemplate> findWorkflowTemplate(WorkflowRun workflowRun) {
        return ofNullable(workflowRun.getWorkflowTemplateId())
            .map(id -> templateCache.get(id, WorkflowTemplate.class));
    }

    @Override
    @CachePut(cacheNames = WORKFLOW_RUN_NAME, key = "#result.id")
    public WorkflowRun create(WorkflowRun workflowRun, boolean requiresPersistence) {
        if (requiresPersistence) {
            lock.lock();
            try {
                applicationManager.getBeanOfType(WorkflowRunRepository.class).save(workflowRun);
            } finally {
                lock.unlock();
            }
        }

        return workflowRun;
    }

    @CacheEvict(cacheNames = WORKFLOW_RUN_NAME, key = "#workflowRun.id")
    public void delete(WorkflowRun workflowRun) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowRunRepository.class).delete(workflowRun);
        } finally {
            lock.unlock();
        }
    }

    public void deleteOldEntriesIfNeeded(boolean force) {
        if (!force && cache.size() < cacheConfig.getWorkflowRunMaxSize()) {
            return;
        }
        final var start = System.currentTimeMillis();
        final var workflowRunRepository = applicationManager.getBeanOfType(WorkflowRunRepository.class);
        final var entries = workflowRunRepository.deleteOldEntries(cacheConfig.getWorkflowRunMaxSize());
        log.debug("{} workflow run(s) deleted from database in {} ms.", entries, System.currentTimeMillis() - start);
    }

    @Override
    protected WorkflowRunFilter createFilter() {
        return new WorkflowRunFilter();
    }

}
