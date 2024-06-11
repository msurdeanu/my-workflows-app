package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.springframework.stereotype.Service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowRunService extends CacheableDataService<WorkflowRun, WorkflowRunFilter> {

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, "workflowRunCacheManager", "workflow-runs");
    }

    @Override
    protected WorkflowRunFilter createFilter() {
        return new WorkflowRunFilter();
    }

}
