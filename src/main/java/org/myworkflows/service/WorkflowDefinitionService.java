package org.myworkflows.service;

import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.event.WorkflowDefinitionOnUpdateEvent;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.springframework.stereotype.Service;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowDefinitionService extends CacheableDataService<WorkflowDefinition, WorkflowDefinitionFilter> {

    public WorkflowDefinitionService(ApplicationManager applicationManager) {
        super(applicationManager, "workflowDefinitionCacheManager", "workflow-definitions");
    }

    public boolean updateDefinition(Integer workflowDefinitionId, String newScript) {
        lock.lock();
        try {
            return ofNullable(cache.get(workflowDefinitionId, WorkflowDefinition.class)).map(workflowDefinition -> {
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

    public void updateName(Integer workflowDefinitionId, String newName) {
        lock.lock();
        try {
            ofNullable(cache.get(workflowDefinitionId, WorkflowDefinition.class)).ifPresent(workflowDefinition -> {
                workflowDefinition.setName(newName);
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowDefinitionOnUpdateEvent.builder().workflowDefinition(workflowDefinition).build());
            });
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowDefinitionFilter createFilter() {
        return new WorkflowDefinitionFilter();
    }


}
