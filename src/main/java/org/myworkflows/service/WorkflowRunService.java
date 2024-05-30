package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.filter.ExecutionContextFilter;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowRunService extends CacheableDataService<ExecutionContext, ExecutionContextFilter> {

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, "workflowRunCacheManager", "workflow-runs");
    }

    @Override
    protected ExecutionContextFilter createFilter() {
        return new ExecutionContextFilter();
    }

}
