package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowRunnable;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Service;

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
public final class WorkflowSchedulerService {

    private final Map<String, ScheduledFuture<?>> scheduledFutureMap = new HashMap<>();

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    public void schedule(final WorkflowDefinition workflowDefinition, final String cron) {
        final var scheduledTask = applicationManager.getBeanOfType(TaskScheduler.class)
                .schedule(new WorkflowRunnable(applicationManager, workflowDefinition),
                        new CronTrigger(cron, TimeZone.getTimeZone(TimeZone.getDefault().getID())));

        lock.lock();
        try {
            unschedule(workflowDefinition);
            scheduledFutureMap.put(workflowDefinition.getId(), scheduledTask);
        } finally {
            lock.unlock();
        }

        log.info("Workflow ID '{}' has been scheduled. Running frequency is '{}'", workflowDefinition.getId(), cron);
    }

    public void unschedule(final WorkflowDefinition workflowDefinition) {
        lock.lock();
        try {
            ofNullable(scheduledFutureMap.get(workflowDefinition.getId())).ifPresent(task -> {
                task.cancel(true);
                scheduledFutureMap.remove(workflowDefinition.getId());

                log.info("Workflow ID '{}' has been unscheduled.", workflowDefinition.getId());
            });
        } finally {
            lock.unlock();
        }
    }

}
