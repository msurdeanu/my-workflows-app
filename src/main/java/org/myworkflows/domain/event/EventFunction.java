package org.myworkflows.domain.event;

import java.util.Optional;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@FunctionalInterface
public interface EventFunction<T> {

    Optional<Event> apply(T item);

}
