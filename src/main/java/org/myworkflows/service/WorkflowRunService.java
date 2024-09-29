package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowRunService extends CacheableDataService<WorkflowRun, WorkflowRunFilter> {

    private final InternalCache templateCache;

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_RUN);
        templateCache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName());
    }

    public Optional<WorkflowTemplate> findWorkflowTemplate(WorkflowRun workflowRun) {
        return ofNullable(workflowRun.getWorkflowTemplateId())
            .map(id -> templateCache.get(id, WorkflowTemplate.class));
    }

    @Override
    protected WorkflowRunFilter createFilter() {
        return new WorkflowRunFilter();
    }

}
