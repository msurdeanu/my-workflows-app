package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.domain.CacheableEntry;
import org.myworkflows.domain.event.EventFunction;
import org.myworkflows.domain.filter.Filter;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class CacheableDataService<T, F extends Filter<T>> {

    protected final Lock lock = new ReentrantLock();

    protected final ApplicationManager applicationManager;

    protected final InternalCache<Object, Object> cache;

    public CacheableDataService(ApplicationManager applicationManager, String cacheName) {
        this.applicationManager = applicationManager;
        cache = applicationManager.getBeanOfTypeAndName(InternalCache.class, cacheName);
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
        cache.putFirst(entry.getCacheableKey(), entry);
    }

    public void addToCacheAtTheEnd(CacheableEntry entry) {
        cache.putLast(entry.getCacheableKey(), entry);
    }

    @SuppressWarnings("uncheked")
    protected void doAction(Object key, Consumer<T> action, EventFunction<T> eventFunction) {
        lock.lock();
        try {
            ofNullable(cache.get(key)).ifPresent(item -> {
                action.accept((T) item);
                eventFunction.apply((T) item)
                    .ifPresent(event -> applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(event));
            });
        } finally {
            lock.unlock();
        }
    }

    protected abstract F createFilter();

}
