package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.event.WorkflowDefinitionOnUpdateEvent;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Service;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowDefinitionService extends AbstractDataService<WorkflowDefinition, WorkflowDefinitionFilter> {

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    private final CaffeineCache cache;

    public WorkflowDefinitionService(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;
        cache = (CaffeineCache) applicationManager
            .getBeanOfTypeAndName(CacheManager.class, "workflowDefinitionCacheManager")
            .getCache("workflow-definitions");
    }

    public void addToCache(WorkflowDefinition workflowDefinition) {
        cache.put(workflowDefinition.getId(), workflowDefinition);
    }

    @Override
    public Stream<WorkflowDefinition> getAllItems() {
        return cache.getNativeCache().asMap().values().stream()
            .filter(item -> item instanceof WorkflowDefinition)
            .map(item -> (WorkflowDefinition) item);
    }

    public boolean changeDefinition(Integer id, String newScript) {
        lock.lock();
        try {
            return ofNullable(cache.get(id, WorkflowDefinition.class)).map(workflowDefinition -> {
                workflowDefinition.setScript(fromJsonToObject(newScript, WorkflowDefinitionScript.class));
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowDefinitionOnUpdateEvent.builder().workflowDefinition(workflowDefinition).build());
                return true;
            }).orElse(false);
        } catch (Exception notUsed) {
            return false;
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowDefinitionFilter createFilter() {
        return new WorkflowDefinitionFilter();
    }

}
