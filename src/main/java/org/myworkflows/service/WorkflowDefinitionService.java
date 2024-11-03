package org.myworkflows.service;

import jakarta.transaction.Transactional;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowDefinitionService extends CacheableDataService<WorkflowDefinition, WorkflowDefinitionFilter> implements LoaderService {

    public WorkflowDefinitionService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_DEFINITION);
    }

    @Order(25)
    @EventListener(ApplicationReadyEvent.class)
    @Override
    public void load() {
        applicationManager.getBeanOfType(WorkflowDefinitionRepository.class)
            .findAll()
            .forEach(this::addToCache);
    }

    @Transactional
    public int delete(WorkflowDefinition workflowDefinition) {
        lock.lock();
        try {
            final var rowsAffected = applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).deleteIfNotUsed(workflowDefinition.getId());
            if (rowsAffected > 0) {
                cache.evict(workflowDefinition.getId());
            }
            return rowsAffected;
        } finally {
            lock.unlock();
        }
    }

    public void updateDefinition(WorkflowDefinition workflowDefinition, String newScript) {
        lock.lock();
        try {
            workflowDefinition.setScript(fromJsonToObject(newScript, WorkflowDefinitionScript.class));
            applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
        } finally {
            lock.unlock();
        }
    }

    public void updateName(WorkflowDefinition workflowDefinition, String newName) {
        lock.lock();
        try {
            workflowDefinition.setName(newName);
            applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowDefinitionFilter createFilter() {
        return new WorkflowDefinitionFilter();
    }

}
