package org.myworkflows.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.stats.CacheStats;
import lombok.Builder;
import lombok.Getter;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class InternalCache<K, V> {

    @Getter
    private final String name;

    private final Cache<K, V> cache;

    private final boolean ordered;

    private final LinkedList<K> keysOrdered;

    private final Lock lock = new ReentrantLock();

    @SuppressWarnings("unchecked")
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
                        removeFromTheEnd((K) key);
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

    public long estimatedSize() {
        return cache.estimatedSize();
    }

    public Optional<V> find(K key) {
        return ofNullable(cache.getIfPresent(key));
    }

    public V get(K key) {
        return cache.getIfPresent(key);
    }

    @SuppressWarnings("unchecked")
    public <T> T get(K key, Class<T> type) {
        return (T) find(key)
            .filter(item -> type.isAssignableFrom(item.getClass()))
            .orElse(null);
    }

    public Collection<V> getAllValues() {
        if (ordered) {
            lock.lock();
            try {
                final var result = new ArrayList<V>((int) cache.estimatedSize());
                keysOrdered.forEach(key -> find(key).ifPresent(result::add));
                return result;
            } finally {
                lock.unlock();
            }
        }
        return cache.asMap().values();
    }

    public void invalidate(K key) {
        // for ordered cache, removal listener will remove stuff from the LinkedList afterward
        cache.invalidate(key);
    }

    public void putFirst(K key, V value) {
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

    public void putLast(K key, V value) {
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

    public CacheStats stats() {
        return cache.stats();
    }

    private void removeFromTheEnd(K key) {
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
