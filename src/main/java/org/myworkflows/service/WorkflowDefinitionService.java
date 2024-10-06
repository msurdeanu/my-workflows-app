package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowDefinitionService extends CacheableDataService<WorkflowDefinition, WorkflowDefinitionFilter> {

    public WorkflowDefinitionService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_DEFINITION);
    }

    public void delete(WorkflowDefinition workflowDefinition) {
        lock.lock();
        try {
            cache.evict(workflowDefinition.getId());
            // TODO: Proceed with a soft delete
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

    public void updateParameter(WorkflowDefinition workflowDefinition, Stream<WorkflowParameter> newParameters) {
        lock.lock();
        try {
            workflowDefinition.setWorkflowParameters(newParameters.collect(Collectors.toList()));
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
