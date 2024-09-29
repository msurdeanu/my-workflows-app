package org.myworkflows.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.stats.CacheStats;
import lombok.Builder;
import lombok.Getter;
import org.springframework.cache.support.SimpleValueWrapper;
import org.springframework.lang.Nullable;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class InternalCache implements org.springframework.cache.Cache {

    private final String name;

    private final Cache<Object, Object> cache;

    private final boolean ordered;
    private final LinkedList<Object> keysOrdered;
    private final Lock lock = new ReentrantLock();

    public InternalCache(String name, InternalCacheConfig config) {
        this.name = name;
        this.ordered = config.isOrdered();

        final var caffeine = Caffeine.newBuilder().recordStats();
        if (config.getMaxSize() > 0) {
            caffeine.maximumSize(config.getMaxSize());
        }
        final var expireAfterWrite = config.getExpireAfterWrite();
        if (expireAfterWrite != null && expireAfterWrite.getSeconds() > 0) {
            caffeine.expireAfterWrite(expireAfterWrite);
        }
        if (ordered) {
            keysOrdered = new LinkedList<>();
            caffeine.removalListener((key, value, cause) -> {
                if (cause != RemovalCause.REPLACED) {
                    lock.lock();
                    try {
                        removeFromTheEnd(key);
                    } finally {
                        lock.unlock();
                    }
                }
            });
        } else {
            keysOrdered = null;
        }
        cache = caffeine.build();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Object getNativeCache() {
        return cache;
    }

    @Override
    @Nullable
    public ValueWrapper get(Object key) {
        return ofNullable(cache.getIfPresent(key))
            .map(SimpleValueWrapper::new)
            .orElse(null);
    }

    @Override
    @Nullable
    public <T> T get(Object key, Class<T> type) {
        assert type != null;
        return ofNullable(cache.getIfPresent(key))
            .filter(type::isInstance)
            .map(type::cast)
            .orElse(null);
    }

    @Override
    public <T> T get(Object key, Callable<T> valueLoader) {
        // TODO
        return null;
    }

    @Override
    public void put(Object key, Object value) {
        if (ordered) {
            lock.lock();
            try {
                find(key).ifPresentOrElse(item -> {
                    removeFromTheEnd(key);
                    if (item != value) {
                        cache.put(key, value);
                    }
                }, () -> cache.put(key, value));
                keysOrdered.addFirst(key);
            } finally {
                lock.unlock();
            }
        } else {
            cache.put(key, value);
        }
    }

    public void putAtTheEnd(Object key, Object value) {
        if (ordered) {
            lock.lock();
            try {
                find(key).ifPresentOrElse(item -> {
                    removeFromTheEnd(key);
                    if (item != value) {
                        cache.put(key, value);
                    }
                }, () -> cache.put(key, value));
                keysOrdered.addLast(key);
            } finally {
                lock.unlock();
            }
        } else {
            cache.put(key, value);
        }
    }

    @Override
    public void evict(Object key) {
        cache.invalidate(key);
    }

    @Override
    public void clear() {
        cache.invalidateAll();
    }

    public long estimatedSize() {
        return cache.estimatedSize();
    }

    public Optional<Object> find(Object key) {
        return ofNullable(cache.getIfPresent(key));
    }

    public Collection<Object> getAllValues() {
        if (ordered) {
            lock.lock();
            try {
                final var result = new ArrayList<>((int) cache.estimatedSize());
                keysOrdered.forEach(key -> find(key).ifPresent(result::add));
                return result;
            } finally {
                lock.unlock();
            }
        }
        return cache.asMap().values();
    }

    public CacheStats stats() {
        return cache.stats();
    }

    private void removeFromTheEnd(Object key) {
        final var iterator = keysOrdered.listIterator(keysOrdered.size());
        while (iterator.hasPrevious()) {
            final var element = iterator.previous();
            if (element.equals(key)) {
                iterator.remove();
                break;
            }
        }
    }

    @Builder
    @Getter
    public static class InternalCacheConfig {
        private boolean ordered;
        private long maxSize;
        private Duration expireAfterWrite;
    }

}
