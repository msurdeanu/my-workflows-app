package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.filter.WorkflowParameterFilter;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowParameterService extends CacheableDataService<WorkflowParameter, WorkflowParameterFilter> {

    public WorkflowParameterService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_PARAMETER);
    }

    @Override
    protected WorkflowParameterFilter createFilter() {
        return new WorkflowParameterFilter();
    }

}
