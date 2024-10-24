package org.myworkflows;

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
public final class ApplicationManager {

    private final ApplicationContext applicationContext;

    public <T> T getBeanOfType(Class<T> clazz) {
        return applicationContext.getBean(clazz);
    }

    public <T> T getBeanOfTypeAndName(Class<T> clazz, String name) {
        return applicationContext.getBean(name, clazz);
    }

    public <T> Collection<T> getBeansOfType(Class<T> clazz) {
        return applicationContext.getBeansOfType(clazz).values();
    }

    public <T> Stream<T> getBeansOfTypeAsStream(Class<T> clazz) {
        return getBeansOfType(clazz).stream();
    }

}