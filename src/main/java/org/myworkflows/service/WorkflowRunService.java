package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.filter.ExecutionContextFilter;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Service;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowRunService extends AbstractDataService<ExecutionContext, ExecutionContextFilter> {

    private final CaffeineCache cache;

    public WorkflowRunService(ApplicationManager applicationManager) {
        cache = (CaffeineCache) applicationManager
            .getBeanOfTypeAndName(CacheManager.class, "workflowRunCacheManager")
            .getCache("workflow-runs");
    }

    public void add(ExecutionContext executionContext) {
        cache.put(executionContext.getWorkflowId(), executionContext);
    }

    @Override
    protected ExecutionContextFilter createFilter() {
        return new ExecutionContextFilter();
    }

    @Override
    public Stream<ExecutionContext> getAllItems() {
        return cache.getNativeCache().asMap().values().stream()
            .filter(item -> item instanceof ExecutionContext)
            .map(item -> (ExecutionContext) item);
    }

}
