package org.myworkflows.domain.event;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface EventListener<E extends Event> {

    void onEventReceived(E event);

    Class<E> getEventType();

}

