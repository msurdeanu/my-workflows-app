package org.myworkflows.service;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.Workflow;
import org.myworkflows.domain.WorkflowRunnable;
import org.springframework.beans.factory.annotation.Qualifier;
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
public final class WorkflowSchedulerService {

    private final Map<String, ScheduledFuture<?>> scheduledFutureMap = new HashMap<>();

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    private final TaskScheduler taskScheduler;

    public WorkflowSchedulerService(ApplicationManager applicationManager, @Qualifier("workflow-scheduler-pool") TaskScheduler taskScheduler) {
        this.applicationManager = applicationManager;
        this.taskScheduler = taskScheduler;
    }

    public void schedule(final Workflow workflow) {
        final var scheduler = workflow.getScheduler();
        final var scheduledTask = taskScheduler.schedule(new WorkflowRunnable(applicationManager, workflow),
                new CronTrigger(scheduler, TimeZone.getTimeZone(TimeZone.getDefault().getID())));

        lock.lock();
        try {
            unschedule(workflow);
            scheduledFutureMap.put(workflow.getId(), scheduledTask);
        } finally {
            lock.unlock();
        }

        log.info("Workflow ID '{}' has been scheduled. Running frequency is '{}'", workflow.getId(), scheduler);
    }

    public void unschedule(final Workflow workflow) {
        lock.lock();
        try {
            ofNullable(scheduledFutureMap.get(workflow.getId())).ifPresent(task -> {
                task.cancel(true);
                scheduledFutureMap.remove(workflow.getId());

                log.info("Workflow ID '{}' has been unscheduled.", workflow.getId());
            });
        } finally {
            lock.unlock();
        }
    }

}
