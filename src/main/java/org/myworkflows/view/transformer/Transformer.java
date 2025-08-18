package org.myworkflows.view.transformer;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@FunctionalInterface
public interface Transformer<K, T> {

    T transform(K input);

}
