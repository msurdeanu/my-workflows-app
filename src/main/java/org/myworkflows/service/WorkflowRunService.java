package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowRunService extends CacheableDataService<WorkflowRun, WorkflowRunFilter> {

    private final CaffeineCache templateCache;

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, "workflowRunCacheManager", "workflow-runs");
        templateCache = (CaffeineCache) applicationManager
            .getBeanOfTypeAndName(CacheManager.class, "workflowTemplateCacheManager")
            .getCache("workflow-templates");
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
