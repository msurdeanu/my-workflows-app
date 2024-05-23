package org.myworkflows.service;

import lombok.NonNull;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.WorkflowTemplateOnDeleteEvent;
import org.myworkflows.domain.event.WorkflowTemplateOnUpdateEvent;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Service;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowTemplateService extends AbstractDataService<WorkflowTemplate, WorkflowTemplateFilter> {

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    private final CaffeineCache cache;

    public WorkflowTemplateService(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;
        cache = (CaffeineCache) applicationManager
            .getBeanOfTypeAndName(CacheManager.class, "workflowTemplateCacheManager")
            .getCache("workflow-templates");
    }

    @Override
    public Stream<WorkflowTemplate> getAllItems() {
        return cache.getNativeCache().asMap().values().stream()
            .filter(item -> item instanceof WorkflowTemplate)
            .map(item -> (WorkflowTemplate) item);
    }

    public void loadAndSchedule(@NonNull WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            final var previousWorkflowTemplate = cache.get(workflowTemplate.getId(), WorkflowTemplate.class);
            ofNullable(previousWorkflowTemplate)
                .filter(WorkflowTemplate::isEnabledForScheduling)
                .ifPresent(oldItem -> applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate));
            cache.evictIfPresent(workflowTemplate.getId());
            cache.put(workflowTemplate.getId(), workflowTemplate);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .schedule(workflowTemplate);
            }
        } finally {
            lock.unlock();
        }
    }

    public void changeActivation(@NonNull Integer workflowId) {
        lock.lock();
        try {
            ofNullable(cache.get(workflowId, WorkflowTemplate.class)).ifPresent(workflowTemplate -> {
                workflowTemplate.toggleOnEnabling();
                if (workflowTemplate.isEnabled()) {
                    applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                        .unschedule(workflowTemplate);
                } else {
                    applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                        .schedule(workflowTemplate);
                }
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
            });
        } finally {
            lock.unlock();
        }
    }

    public void changeCron(Integer workflowId, String newCron) {
        lock.lock();
        try {
            ofNullable(cache.get(workflowId, WorkflowTemplate.class)).ifPresent(workflowTemplate -> {
                if (workflowTemplate.isEnabled()) {
                    applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                        .unschedule(workflowTemplate);
                }
                workflowTemplate.setCron(newCron);
                if (workflowTemplate.isEnabled()) {
                    applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                        .schedule(workflowTemplate);
                }
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
            });
        } finally {
            lock.unlock();
        }
    }

    public boolean changeName(Integer workflowId, String newName) {
        lock.lock();
        try {
            return ofNullable(cache.get(workflowId, WorkflowTemplate.class)).map(workflowTemplate -> {
                workflowTemplate.setName(newName);
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
                return true;
            }).orElse(false);
        } catch (Exception notUsed) {
            return false;
        } finally {
            lock.unlock();
        }
    }

    public void updateDefinition(Integer workflowId, Stream<WorkflowDefinition> newDefinitions) {
        lock.lock();
        try {
            ofNullable(cache.get(workflowId, WorkflowTemplate.class)).ifPresent(workflowTemplate -> {
                workflowTemplate.setWorkflowDefinitions(newDefinitions.collect(Collectors.toList()));
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
            });
        } finally {
            lock.unlock();
        }
    }

    public void delete(Integer workflowId) {
        lock.lock();
        try {
            final var workflowTemplate = cache.get(workflowId, WorkflowTemplate.class);
            cache.evictIfPresent(workflowId);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate);
            }
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowTemplateOnDeleteEvent.builder().workflowTemplate(workflowTemplate).build());
        } finally {
            lock.unlock();
        }
    }

    public void scheduleNow(Integer workflowId) {
        ofNullable(cache.get(workflowId, WorkflowTemplate.class)).ifPresent(workflowTemplate -> {
            applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                .scheduleNowAsync(workflowTemplate);
        });
    }

    @Override
    protected WorkflowTemplateFilter createFilter() {
        return new WorkflowTemplateFilter();
    }

}
