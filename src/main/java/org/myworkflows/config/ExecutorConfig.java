package org.myworkflows.config;

import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.concurrent.ConcurrentTaskScheduler;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Setter
@Configuration("executorConfig")
@ConfigurationProperties(prefix = "my-workflows.config.executor")
@EnableScheduling
public class ExecutorConfig {

    @Bean(name = "workflow-scheduler-pool")
    public TaskScheduler workflowSchedulerPool() {
        final var virtualThreadFactory = Thread.ofVirtual().factory();
        final var virtualThreadScheduler = Executors.newScheduledThreadPool(1, virtualThreadFactory);
        return new ConcurrentTaskScheduler(virtualThreadScheduler);
    }

    @Bean(name = "workflow-pool")
    public ExecutorService workflowPool() {
        return Executors.newVirtualThreadPerTaskExecutor();
    }

}
