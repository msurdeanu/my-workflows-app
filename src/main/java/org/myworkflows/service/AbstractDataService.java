package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import org.myworkflows.domain.filter.Filter;

import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class AbstractDataService<T, F extends Filter<T>> {

    public Stream<T> findBy(Query<T, F> query) {
        return query.getFilter()
            .map(filter -> getAll(filter, query.getOffset(), query.getLimit()))
            .orElseGet(this::getAll);
    }

    public int countBy(Query<T, F> query) {
        return query.getFilter()
            .map(this::getAllSize)
            .orElseGet(this::getAllSize)
            .intValue();
    }

    public Stream<T> getAll() {
        return getAll(createFilter(), 0, Long.MAX_VALUE);
    }

    public Stream<T> getAll(F filter, long offset, long limit) {
        return getAllItems()
            .filter(filter.getFilterPredicate())
            .skip(offset)
            .limit(limit);
    }

    public long getAllSize() {
        return getAll().count();
    }

    public long getAllSize(F filter) {
        return getAll(filter, 0, Long.MAX_VALUE).count();
    }

    public abstract Stream<T> getAllItems();

    protected abstract F createFilter();

}
