package org.myworkflows.service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface LoaderService {

    void load();

    default void unload() {
        // Nothing to do by default
    }

}
