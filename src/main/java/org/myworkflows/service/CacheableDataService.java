package org.myworkflows.service;

import com.google.common.reflect.TypeToken;
import com.vaadin.flow.data.provider.Query;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.CacheableEntry;
import org.myworkflows.domain.event.EventFunction;
import org.myworkflows.domain.filter.Filter;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;

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

    protected final CaffeineCache cache;

    private final Class<T> typeClass;

    public CacheableDataService(ApplicationManager applicationManager, String cacheManagerName, String cacheName) {
        this.applicationManager = applicationManager;
        cache = (CaffeineCache) applicationManager
            .getBeanOfTypeAndName(CacheManager.class, cacheManagerName)
            .getCache(cacheName);
        TypeToken<T> typeToken = new TypeToken<>(getClass()) {
        };
        this.typeClass = (Class<T>) typeToken.getRawType();
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

    public Stream<T> getAllItems() {
        return cache.getNativeCache().asMap().values().stream()
            .filter(typeClass::isInstance)
            .map(typeClass::cast);
    }

    public void addToCache(CacheableEntry entry) {
        cache.put(entry.getCacheableKey(), entry);
    }

    protected void doAction(Object key, Consumer<T> action, EventFunction<T> eventFunction) {
        lock.lock();
        try {
            ofNullable(cache.get(key, typeClass)).ifPresent(item -> {
                action.accept(item);
                eventFunction.apply(item)
                    .ifPresent(event -> applicationManager.getBeanOfType(EventBroadcaster.class).broadcast(event));
            });
        } finally {
            lock.unlock();
        }
    }

    protected abstract F createFilter();

}
