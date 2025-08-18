package org.myworkflows.service;

import jakarta.transaction.Transactional;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_DEFINITION_NAME;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
public class WorkflowDefinitionService extends CacheableDataService<WorkflowDefinition, WorkflowDefinitionFilter>
    implements ServiceCreator<WorkflowDefinition> {

    public WorkflowDefinitionService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_DEFINITION);
    }

    @Override
    @CachePut(cacheNames = WORKFLOW_DEFINITION_NAME, key = "#result.id")
    public WorkflowDefinition create(WorkflowDefinition workflowDefinition, boolean requiresPersistence) {
        if (requiresPersistence) {
            lock.lock();
            try {
                applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
            } finally {
                lock.unlock();
            }
        }

        return workflowDefinition;
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

    @Transactional
    public void updateDefinition(WorkflowDefinition workflowDefinition, String newScript) {
        lock.lock();
        try {
            workflowDefinition.setScript(toObject(newScript, WorkflowDefinitionScript.class));
            applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
            applicationManager.getBeanOfType(WorkflowTemplateService.class).propagateInternalUpdate(workflowDefinition);
        } finally {
            lock.unlock();
        }
    }

    @Transactional
    public void updateName(WorkflowDefinition workflowDefinition, String newName) {
        lock.lock();
        try {
            workflowDefinition.setName(newName);
            applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
            applicationManager.getBeanOfType(WorkflowTemplateService.class).propagateInternalUpdate(workflowDefinition);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowDefinitionFilter createFilter() {
        return new WorkflowDefinitionFilter();
    }

}
