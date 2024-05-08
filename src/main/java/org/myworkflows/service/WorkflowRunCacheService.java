package org.myworkflows.service;

import org.myworkflows.domain.ExecutionContext;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Service;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowRunCacheService {

    private final CaffeineCache cache;

    public WorkflowRunCacheService(CacheManager cacheManager) {
        cache = (CaffeineCache) cacheManager.getCache("workflow-runs");
    }

    public void add(ExecutionContext executionContext) {
        cache.put(executionContext.getWorkflowId(), executionContext);
    }

    public Stream<ExecutionContext> getAllValuesAsStream() {
        return cache.getNativeCache().asMap().values().stream()
            .filter(item -> item instanceof ExecutionContext)
            .map(item -> (ExecutionContext) item);
    }

}
