package org.myworkflows.service;

import lombok.NonNull;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.EventFunction;
import org.myworkflows.domain.event.WorkflowTemplateOnDeleteEvent;
import org.myworkflows.domain.event.WorkflowTemplateOnUpdateEvent;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.of;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowTemplateService extends CacheableDataService<WorkflowTemplate, WorkflowTemplateFilter> {

    private static final EventFunction<WorkflowTemplate> UPDATE_EVENT_FUNCTION = item ->
        of(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(item).build());
    private static final EventFunction<WorkflowTemplate> DELETE_EVENT_FUNCTION = item ->
        of(WorkflowTemplateOnDeleteEvent.builder().workflowTemplate(item).build());

    public WorkflowTemplateService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_TEMPLATE);
    }

    public void loadAndSchedule(@NonNull WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            ofNullable(cache.get(workflowTemplate.getId(), WorkflowTemplate.class))
                .filter(WorkflowTemplate::isEnabledForScheduling)
                .ifPresent(oldItem -> applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate));
            cache.evict(workflowTemplate.getId());
            cache.put(workflowTemplate.getId(), workflowTemplate);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .schedule(workflowTemplate);
            }
        } finally {
            lock.unlock();
        }
    }

    public void delete(Integer workflowTemplateId) {
        doAction(workflowTemplateId, workflowTemplate -> {
            cache.evict(workflowTemplate.getId());
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate);
            }
        }, DELETE_EVENT_FUNCTION);
    }

    public void updateActivation(Integer workflowTemplateId) {
        doAction(workflowTemplateId, workflowTemplate -> {
            workflowTemplate.toggleOnEnabling();
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).unschedule(workflowTemplate);
            } else {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).schedule(workflowTemplate);
            }
        }, UPDATE_EVENT_FUNCTION);
    }

    public void updateDefinition(Integer workflowTemplateId, Stream<WorkflowDefinition> newDefinitions) {
        doAction(
            workflowTemplateId,
            workflowTemplate -> workflowTemplate.setWorkflowDefinitions(newDefinitions.collect(Collectors.toList())),
            UPDATE_EVENT_FUNCTION
        );
    }

    public void updateNameAndCron(Integer workflowTemplateId, String newName, String newCron) {
        doAction(workflowTemplateId, workflowTemplate -> {
            workflowTemplate.setName(newName);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate);
            }
            workflowTemplate.setCron(newCron);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .schedule(workflowTemplate);
            }
        }, UPDATE_EVENT_FUNCTION);
    }

    public void scheduleNow(Integer workflowTemplateId) {
        ofNullable(cache.get(workflowTemplateId, WorkflowTemplate.class)).ifPresent(workflowTemplate -> {
            applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                .scheduleNowAsync(workflowTemplate);
        });
    }

    @Override
    protected WorkflowTemplateFilter createFilter() {
        return new WorkflowTemplateFilter();
    }

}
