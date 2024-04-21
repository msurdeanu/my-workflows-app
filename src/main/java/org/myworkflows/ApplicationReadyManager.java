package org.myworkflows;

import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.event.Event;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Component
@RequiredArgsConstructor
public class ApplicationReadyManager {

    private final ApplicationManager applicationManager;

    @EventListener
    @Order(1)
    public void registerAllListeners(final ApplicationReadyEvent event) {
        event.getApplicationContext()
                .getBeansOfType(org.myworkflows.domain.event.EventListener.class)
                .forEach(this::registerListener);
    }

    private void registerListener(final String key, final org.myworkflows.domain.event.EventListener<Event> value) {
        applicationManager.getBeanOfType(EventBroadcaster.class).register(value::onEventReceived, value.getEventType());
    }

}
