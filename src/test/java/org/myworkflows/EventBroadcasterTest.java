package org.myworkflows;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;

import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class EventBroadcasterTest {

    @Test
    public void whenConsumerIsRegisteredAndEventIsGeneratedThenEverythingWorksAsExpected() throws InterruptedException {
        final var token = UUID.randomUUID();
        final var workflowDefinitionOnProgressEvent = WorkflowDefinitionOnProgressEvent.builder().token(token).build();
        final var threadPoolExecutor = new ThreadPoolExecutor(1, 1, 5L, TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(10), new ThreadPoolExecutor.CallerRunsPolicy());
        threadPoolExecutor.allowCoreThreadTimeOut(true);

        final var eventBroadcaster = new EventBroadcaster(threadPoolExecutor);
        eventBroadcaster.register(event -> {
            final var onProgressEvent = (WorkflowDefinitionOnProgressEvent) event;
            assertEquals(token, onProgressEvent.getToken());
        }, WorkflowDefinitionOnProgressEvent.class);

        eventBroadcaster.broadcast(workflowDefinitionOnProgressEvent);
        threadPoolExecutor.shutdown();
        assertTrue(threadPoolExecutor.awaitTermination(10, TimeUnit.SECONDS));
    }

}
