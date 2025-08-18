package org.myworkflows.domain.filter;

import java.util.function.Predicate;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface Filter<T> {

    Predicate<T> getFilterPredicate();

}
