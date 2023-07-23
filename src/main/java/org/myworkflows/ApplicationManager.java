package org.myworkflows;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Component
@RequiredArgsConstructor
public class ApplicationManager {

    private final ApplicationContext applicationContext;

    @Getter
    private final EventBroadcaster eventBroadcaster;

    public <T> Collection<T> getBeansOfType(final Class<T> clazz) {
        return applicationContext.getBeansOfType(clazz).values();
    }

    public <T> Stream<T> getBeansOfTypeAsStream(final Class<T> clazz) {
        return getBeansOfType(clazz).stream();
    }

}