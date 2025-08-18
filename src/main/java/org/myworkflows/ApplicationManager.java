package org.myworkflows;

import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Component
@RequiredArgsConstructor
public final class ApplicationManager {

    private final ApplicationContext applicationContext;

    public <T> T getBeanOfType(Class<T> clazz) {
        return applicationContext.getBean(clazz);
    }

}