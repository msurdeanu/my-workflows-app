package org.myworkflows.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Configuration("executorConfig")
@ConfigurationProperties(prefix = "my-workflows.config.executor")
@EnableScheduling
public class ExecutorConfig {

    private final int workflowSchedulerPoolSize = 2;

    private final int workflowPoolSize = 4;
    private final int workflowPoolCapacity = 1_000;

    private final int eventPoolSize = 2;
    private final int eventPoolCapacity = 1_000;

    @Bean(name = "workflow-scheduler-pool")
    public TaskScheduler workflowSchedulerPool() {
        final var threadPoolTaskScheduler = new ThreadPoolTaskScheduler();
        threadPoolTaskScheduler.setPoolSize(workflowSchedulerPoolSize);
        threadPoolTaskScheduler.setThreadNamePrefix("workflow-scheduler-");
        threadPoolTaskScheduler.initialize();

        return threadPoolTaskScheduler;
    }

    @Bean(name = "workflow-pool")
    public ThreadPoolExecutor workflowPool() {
        final var threadPoolExecutor = new ThreadPoolExecutor(workflowPoolSize, workflowPoolSize,
                60L, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(workflowPoolCapacity),
                new ThreadPoolExecutor.CallerRunsPolicy());
        threadPoolExecutor.allowCoreThreadTimeOut(true);
        return threadPoolExecutor;
    }

    @Bean(name = "event-pool")
    public ThreadPoolExecutor eventPool() {
        final var threadPoolExecutor = new ThreadPoolExecutor(eventPoolSize, eventPoolSize,
                60L, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(eventPoolCapacity),
                new ThreadPoolExecutor.CallerRunsPolicy());
        threadPoolExecutor.allowCoreThreadTimeOut(true);
        return threadPoolExecutor;
    }

}
