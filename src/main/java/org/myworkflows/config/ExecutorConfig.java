package org.myworkflows.config;

import org.springframework.beans.factory.annotation.Qualifier;
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

    @Bean
    @Qualifier("workflow-scheduler-pool")
    public TaskScheduler workflowSchedulerPool() {
        final var threadPoolTaskScheduler = new ThreadPoolTaskScheduler();
        threadPoolTaskScheduler.setPoolSize(2);
        threadPoolTaskScheduler.setThreadNamePrefix("workflow-scheduler-");
        threadPoolTaskScheduler.initialize();

        return threadPoolTaskScheduler;
    }

    @Bean
    @Qualifier("workflow-event-pool")
    public ThreadPoolExecutor workflowEventPool() {
        final var threadPoolExecutor = new ThreadPoolExecutor(2, 2,
                60L, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(100),
                new ThreadPoolExecutor.CallerRunsPolicy());
        threadPoolExecutor.allowCoreThreadTimeOut(true);
        return threadPoolExecutor;
    }

}
