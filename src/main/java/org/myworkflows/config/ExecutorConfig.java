package org.myworkflows.config;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
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
@EnableScheduling
public class ExecutorConfig {

    @Bean
    @Qualifier("workflow-scheduler-pool")
    public ThreadPoolTaskScheduler threadPoolTaskScheduler() {
        final var threadPoolTaskScheduler = new ThreadPoolTaskScheduler();
        threadPoolTaskScheduler.setPoolSize(2);
        threadPoolTaskScheduler.setThreadNamePrefix("workflow-scheduler-pool-");
        threadPoolTaskScheduler.initialize();
        return threadPoolTaskScheduler;
    }

    @Bean
    @Qualifier("workflow-event-pool")
    public ThreadPoolExecutor threadPoolExecutor() {
        return new ThreadPoolExecutor(1, 1,
            30L, TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(100),
            new ThreadPoolExecutor.CallerRunsPolicy());
    }

}
