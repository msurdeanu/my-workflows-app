package org.myworkflows.service;

import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.config.CacheConfig;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.EventListener;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.repository.WorkflowRunRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.core.annotation.Order;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public class WorkflowRunService extends CacheableDataService<WorkflowRun, WorkflowRunFilter>
    implements EventListener<WorkflowDefinitionOnProgressEvent>, LoaderService {

    private final CacheConfig cacheConfig;
    private final InternalCache templateCache;

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_RUN);
        cacheConfig = applicationManager.getBeanOfType(CacheConfig.class);
        templateCache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName());
    }

    @Transactional
    @Override
    public void onEventReceived(WorkflowDefinitionOnProgressEvent event) {
        final var workflowRun = event.getWorkflowRun();

        lock.lock();
        try {
            addToCache(workflowRun);
            if (event.isPersisted()) {
                applicationManager.getBeanOfType(WorkflowRunRepository.class).save(workflowRun);
                if (cache.size() >= cacheConfig.getWorkflowRunMaxSize()) {
                    deleteOldEntries();
                }
            }
        } finally {
            lock.unlock();
        }
    }

    @Override
    public Class<WorkflowDefinitionOnProgressEvent> getEventType() {
        return WorkflowDefinitionOnProgressEvent.class;
    }

    @Order(100)
    @Transactional
    @org.springframework.context.event.EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        deleteOldEntries();
        applicationManager.getBeanOfType(WorkflowRunRepository.class)
            .findByOrderByCreatedDesc(PageRequest.of(0, cacheConfig.getWorkflowRunMaxSize()))
            .reversed()
            .forEach(this::addToCache);
    }

    public Optional<WorkflowTemplate> findWorkflowTemplate(WorkflowRun workflowRun) {
        return ofNullable(workflowRun.getWorkflowTemplateId())
            .map(id -> templateCache.get(id, WorkflowTemplate.class));
    }

    public void delete(WorkflowRun workflowRun) {
        if (workflowRun.getId() == null) {
            return;
        }

        lock.lock();
        try {
            cache.evict(workflowRun.getId());
            applicationManager.getBeanOfType(WorkflowRunRepository.class).delete(workflowRun);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowRunFilter createFilter() {
        return new WorkflowRunFilter();
    }

    private void deleteOldEntries() {
        final var start = System.currentTimeMillis();
        final var workflowRunRepository = applicationManager.getBeanOfType(WorkflowRunRepository.class);
        final var entries = workflowRunRepository.deleteOldEntries(cacheConfig.getWorkflowRunMaxSize());
        if (log.isDebugEnabled()) {
            log.debug("{} workflow run(s) deleted from database in {} ms.", entries, System.currentTimeMillis() - start);
        }
    }

}
