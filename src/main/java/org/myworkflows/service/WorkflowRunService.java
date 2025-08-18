package org.myworkflows.service;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.WorkflowRunRepository;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static java.util.Optional.ofNullable;
import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_RUN_NAME;
import static org.springframework.data.domain.PageRequest.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public class WorkflowRunService extends CacheableDataService<WorkflowRun, WorkflowRunFilter> implements ServiceCreator<WorkflowRun> {

    private final SettingProvider settingProvider;
    private final InternalCache templateCache;

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_RUN);
        settingProvider = applicationManager.getBeanOfType(SettingProvider.class);
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
        final var workflowRunMaxSize = settingProvider.getOrDefault("workflowRunMaxSize", 250);
        if (!force && cache.size() < workflowRunMaxSize) {
            return;
        }
        final var start = System.currentTimeMillis();
        final var workflowRunRepository = applicationManager.getBeanOfType(WorkflowRunRepository.class);
        final var oldestCutoffDate = workflowRunRepository.findOldestCutoffDate(of(workflowRunMaxSize, 1));
        if (oldestCutoffDate.size() != 1) {
            return;
        }
        final var eldestEntries = workflowRunRepository.findOldestEntriesByCutoff(oldestCutoffDate.getFirst());
        if (!eldestEntries.isEmpty()) {
            workflowRunRepository.deleteAllById(eldestEntries);
        }
        if (log.isDebugEnabled()) {
            log.debug("{} run(s) deleted from database in {} ms.", eldestEntries.size(),
                System.currentTimeMillis() - start);
        }
    }

    @Override
    protected WorkflowRunFilter createFilter() {
        return new WorkflowRunFilter();
    }

}
