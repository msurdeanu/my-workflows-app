package org.myworkflows.service;

import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.WorkflowTemplateOnDeleteEvent;
import org.myworkflows.domain.event.WorkflowTemplateOnUpdateEvent;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public final class WorkflowTemplateService extends AbstractDataService<WorkflowTemplate, WorkflowTemplateFilter> {

    private static final Map<Integer, WorkflowTemplate> ALL_WORKFLOWS = new HashMap<>();

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    @Order(10)
    @EventListener(ApplicationReadyEvent.class)
    public void loadAll() {
        applicationManager.getBeanOfType(WorkflowTemplateRepository.class)
            .findAll()
            .forEach(this::loadAndSchedule);
    }

    @Override
    public Stream<WorkflowTemplate> getAllItems() {
        return ALL_WORKFLOWS.values().stream();
    }

    public void loadAndSchedule(@NonNull WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            ofNullable(ALL_WORKFLOWS.put(workflowTemplate.getId(), workflowTemplate))
                .filter(WorkflowTemplate::isEnabledForScheduling)
                .ifPresent(oldItem -> applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate));
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
            ofNullable(ALL_WORKFLOWS.get(workflowId)).ifPresent(workflowTemplate -> {
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
            ofNullable(ALL_WORKFLOWS.get(workflowId)).ifPresent(workflowTemplate -> {
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
            return ofNullable(ALL_WORKFLOWS.get(workflowId)).map(workflowTemplate -> {
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

    public void delete(Integer workflowId) {
        lock.lock();
        try {
            final var workflowTemplate = ALL_WORKFLOWS.remove(workflowId);
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
        ofNullable(ALL_WORKFLOWS.get(workflowId)).ifPresent(workflowTemplate -> {
            applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                .scheduleNowAsync(workflowTemplate);
        });
    }

    @Override
    protected WorkflowTemplateFilter createFilter() {
        return new WorkflowTemplateFilter();
    }

}
