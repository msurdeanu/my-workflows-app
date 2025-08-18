package org.myworkflows.service.loader;

import org.myworkflows.ApplicationManager;
import org.myworkflows.service.ServiceCreator;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface ServiceLoader {

    void load();

    default void unload() {
        // Nothing to do by default
    }

    default <T, S extends ServiceCreator<T>, R extends JpaRepository<T, ?>> void loadByServiceAndRepo(ApplicationManager applicationManager,
                                                                                                      Class<S> serviceClass,
                                                                                                      Class<R> repoClass) {
        final var service = applicationManager.getBeanOfType(serviceClass);
        applicationManager.getBeanOfType(repoClass)
            .findAll()
            .forEach(item -> service.create(item, false));
    }

}
