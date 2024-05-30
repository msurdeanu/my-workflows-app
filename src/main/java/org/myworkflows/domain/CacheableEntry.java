package org.myworkflows.domain;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@FunctionalInterface
public interface CacheableEntry {

    Object key();

}
