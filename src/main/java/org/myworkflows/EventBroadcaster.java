package org.myworkflows;

import com.vaadin.flow.shared.Registration;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.event.Event;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Component
public final class EventBroadcaster {

    private final Map<Class<? extends Event>, List<Consumer<Event>>> consumersMap = new HashMap<>();

    private final Lock lock = new ReentrantLock();

    private final ExecutorService executorService;

    public EventBroadcaster(@Qualifier("workflow-pool") ExecutorService executorService) {
        this.executorService = executorService;
    }

    public void broadcast(Event event) {
        broadcast(event, 0);
    }

    public void broadcast(Event event, long millisDelay) {
        lock.lock();
        try {
            ofNullable(consumersMap.get(event.getClass()))
                    .orElse(List.of())
                    .forEach(consumer -> executorService.execute(() -> {
                        if (millisDelay > 0) {
                            safeSleep(millisDelay);
                        }
                        consumer.accept(event);
                    }));
        } finally {
            lock.unlock();
        }
    }

    public Registration register(Consumer<Event> consumer, Class<? extends Event> acceptedEvent) {
        ofNullable(consumersMap.get(acceptedEvent)).ifPresentOrElse(
                consumers -> consumers.add(consumer),
                () -> consumersMap.put(acceptedEvent, new ArrayList<>(List.of(consumer)))
        );

        log.debug("A new broadcast consumer is registered for event type '{}'.", acceptedEvent.getName());

        return () -> {
            lock.lock();
            try {
                ofNullable(consumersMap.get(acceptedEvent)).ifPresent(items -> {
                    items.remove(consumer);

                    log.debug("A new broadcast consumer is de-registered for event type '{}'.", acceptedEvent.getName());
                });
            } finally {
                lock.unlock();
            }
        };
    }

    private void safeSleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

}
