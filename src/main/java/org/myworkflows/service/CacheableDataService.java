package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.CacheableEntry;
import org.myworkflows.domain.filter.Filter;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class CacheableDataService<T, F extends Filter<T>> {

    protected final Lock lock = new ReentrantLock();

    protected final ApplicationManager applicationManager;

    protected final InternalCache cache;

    public CacheableDataService(ApplicationManager applicationManager, InternalCacheManager.CacheNameEnum cacheName) {
        this.applicationManager = applicationManager;
        cache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(cacheName.getName());
    }

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

    @SuppressWarnings("unchecked")
    public Stream<T> getAllItems() {
        return cache.getAllValues().stream()
            .map(item -> (T) item);
    }

    public void addToCache(CacheableEntry entry) {
        cache.put(entry.getCacheableKey(), entry);
    }

    public void addToCacheAtTheEnd(CacheableEntry entry) {
        cache.putAtTheEnd(entry.getCacheableKey(), entry);
    }

    protected abstract F createFilter();

}
