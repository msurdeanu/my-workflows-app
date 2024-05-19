package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowDefinitionScriptRunnable;
import org.myworkflows.domain.WorkflowTemplate;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class WorkflowTemplateSchedulerService {

    private final Map<Integer, ScheduledFuture<?>> scheduledFutureMap = new HashMap<>();

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    public void schedule(WorkflowTemplate workflowTemplate) {
        final var scheduledTask = applicationManager.getBeanOfType(TaskScheduler.class)
            .schedule(new WorkflowDefinitionScriptRunnable(applicationManager, workflowTemplate.getWorkflowDefinitionScripts()),
                new CronTrigger(workflowTemplate.getCron(), TimeZone.getTimeZone(TimeZone.getDefault().getID())));

        lock.lock();
        try {
            unschedule(workflowTemplate);
            scheduledFutureMap.put(workflowTemplate.getId(), scheduledTask);
        } finally {
            lock.unlock();
        }

        log.info("Workflow template '{}' has been scheduled. Running frequency is '{}'",
            workflowTemplate.getId(), workflowTemplate.getCron());
    }

    public void scheduleNowAsync(WorkflowTemplate workflowTemplate) {
        applicationManager.getBeanOfType(TaskScheduler.class)
            .schedule(new WorkflowDefinitionScriptRunnable(applicationManager, workflowTemplate.getWorkflowDefinitionScripts()), Instant.now());
    }

    public void unschedule(WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            ofNullable(scheduledFutureMap.get(workflowTemplate.getId())).ifPresent(task -> {
                task.cancel(true);
                scheduledFutureMap.remove(workflowTemplate.getId());

                log.info("Workflow template '{}' has been unscheduled.", workflowTemplate.getId());
            });
        } finally {
            lock.unlock();
        }
    }

}
