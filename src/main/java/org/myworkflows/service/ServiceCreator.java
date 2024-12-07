package org.myworkflows.service;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public interface ServiceCreator<T> {

    T create(T item, boolean requiresPersistence);

}
