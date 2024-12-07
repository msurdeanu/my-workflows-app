package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.filter.WorkflowParameterFilter;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowParameterService extends CacheableDataService<WorkflowParameter, WorkflowParameterFilter> implements
    ServiceCreator<WorkflowParameter> {

    public WorkflowParameterService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_PARAMETER);
    }

    @Override
    @CachePut(cacheNames = "workflowParameter", key = "#result.name")
    public WorkflowParameter create(WorkflowParameter workflowParameter, boolean requiresPersistence) {
        if (requiresPersistence) {
            lock.lock();
            try {
                applicationManager.getBeanOfType(WorkflowParameterRepository.class).save(workflowParameter);
            } finally {
                lock.unlock();
            }
        }

        return workflowParameter;
    }

    public void update(WorkflowParameter workflowParameter) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowParameterRepository.class).save(workflowParameter);
        } finally {
            lock.unlock();
        }
    }

    @CacheEvict(cacheNames = "workflowParameter", key = "#workflowParameter.name")
    public void delete(WorkflowParameter workflowParameter) {
        lock.lock();
        try {
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
