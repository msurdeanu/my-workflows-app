package org.myworkflows;

import com.vaadin.flow.shared.Registration;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.myworkflows.domain.event.Event;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.function.Consumer;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class EventBroadcaster {

    private final Map<Class<? extends Event>, List<Consumer<Event>>> consumersMap = new HashMap<>();

    private final ThreadPoolExecutor threadPoolExecutor;

    public synchronized void broadcast(final Event event) {
        ofNullable(consumersMap.get(event.getClass()))
            .orElse(List.of())
            .forEach(consumer -> threadPoolExecutor.execute(() -> consumer.accept(event)));
    }

    public synchronized Registration register(final Consumer<Event> consumer,
                                              final Class<? extends Event> acceptedEvent) {
        ofNullable(consumersMap.get(acceptedEvent)).ifPresentOrElse(
            consumers -> consumers.add(consumer),
            () -> consumersMap.put(acceptedEvent, new ArrayList<>(List.of(consumer)))
        );

        log.debug("A new broadcast consumer is registered for event type '{}'.", acceptedEvent.getName());

        return () -> {
            synchronized (EventBroadcaster.class) {
                ofNullable(consumersMap.get(acceptedEvent)).ifPresent(items -> {
                    items.remove(consumer);

                    log.debug("A new broadcast consumer is de-registered for event type '{}'.", acceptedEvent.getName());
                });
            }
        };
    }

}