package org.myworkflows;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnProgressEvent;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class EventBroadcasterTest {

    @Test
    public void whenConsumerIsRegisteredAndEventIsGeneratedThenEverythingWorksAsExpected() throws InterruptedException {
        final var workflowRun = new WorkflowRun();
        final var workflowDefinitionOnProgressEvent = WorkflowDefinitionOnProgressEvent.builder().workflowRun(workflowRun).build();
        final var threadPoolExecutor = new ThreadPoolExecutor(1, 1, 5L, TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(10), new ThreadPoolExecutor.CallerRunsPolicy());
        threadPoolExecutor.allowCoreThreadTimeOut(true);

        final var eventBroadcaster = new EventBroadcaster(threadPoolExecutor);
        eventBroadcaster.register(event -> {
            final var onProgressEvent = (WorkflowDefinitionOnProgressEvent) event;
            assertEquals(workflowRun.getId(), onProgressEvent.workflowRun().getId());
        }, WorkflowDefinitionOnProgressEvent.class);

        eventBroadcaster.broadcast(workflowDefinitionOnProgressEvent);
        threadPoolExecutor.shutdown();
        assertTrue(threadPoolExecutor.awaitTermination(10, TimeUnit.SECONDS));
    }

    @Test
    public void whenConsumersAreRegisteredWhileBroadcastingThenNothingIsLostOrCorrupted() throws Exception {
        // given a single-threaded executor, so that a barrier task tells when every delivery is done
        final var rounds = 200;
        final var executorService = Executors.newSingleThreadExecutor();
        final var eventBroadcaster = new EventBroadcaster(executorService);
        final var event = WorkflowDefinitionOnProgressEvent.builder().workflowRun(new WorkflowRun()).build();
        final var deliveries = new AtomicInteger();
        final var failures = new CopyOnWriteArrayList<Throwable>();

        // when registrations and broadcasts run concurrently
        final var registrar = Thread.ofPlatform().start(() -> {
            try {
                for (int index = 0; index < rounds; index++) {
                    eventBroadcaster.register(notUsed -> deliveries.incrementAndGet(), WorkflowDefinitionOnProgressEvent.class);
                }
            } catch (Throwable throwable) {
                failures.add(throwable);
            }
        });
        final var broadcaster = Thread.ofPlatform().start(() -> {
            try {
                for (int index = 0; index < rounds; index++) {
                    eventBroadcaster.broadcast(event);
                }
            } catch (Throwable throwable) {
                failures.add(throwable);
            }
        });
        registrar.join();
        broadcaster.join();
        executorService.submit(() -> { }).get(30, TimeUnit.SECONDS);

        // then neither side may have blown up on the shared consumer map
        assertTrue(failures.isEmpty(), () -> "Unexpected failure: " + failures.getFirst());

        // and every consumer must still be registered, so a final broadcast reaches all of them
        deliveries.set(0);
        eventBroadcaster.broadcast(event);
        executorService.submit(() -> { }).get(30, TimeUnit.SECONDS);
        assertEquals(rounds, deliveries.get());
        executorService.shutdown();
    }

}
