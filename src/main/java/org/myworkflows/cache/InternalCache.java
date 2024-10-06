package org.myworkflows.cache;

import lombok.Getter;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.springframework.cache.support.SimpleValueWrapper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.locks.StampedLock;
import java.util.function.Consumer;
import java.util.function.Function;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class InternalCache implements org.springframework.cache.Cache {

    private final String name;

    @Getter
    private final int maxSize;

    @Getter
    private final boolean ordered;

    private final Map<Object, Object> cacheMap = new HashMap<>();

    private final LinkedList<Object> keys = new LinkedList<>();

    private final StampedLock stampedLock = new StampedLock();

    public InternalCache(String name) {
        this(name, Integer.MAX_VALUE, false);
    }

    public InternalCache(String name, int maxSize) {
        this(name, maxSize, false);
    }

    public InternalCache(String name, int maxSize, boolean ordered) {
        this.name = name;
        this.maxSize = Integer.max(1, maxSize);
        this.ordered = ordered;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Object getNativeCache() {
        return cacheMap;
    }

    @Override
    public ValueWrapper get(Object key) {
        final var value = applyFunctionInsideOptimisticReadBlock(key, cacheMap::get);
        return ofNullable(value)
            .map(SimpleValueWrapper::new)
            .orElse(null);
    }

    @Override
    public <T> T get(Object key, Class<T> type) {
        assert type != null;
        final var value = applyFunctionInsideOptimisticReadBlock(key, cacheMap::get);
        return ofNullable(value)
            .filter(type::isInstance)
            .map(type::cast)
            .orElse(null);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T get(Object key, Callable<T> valueLoader) {
        final var value = applyFunctionInsideOptimisticReadBlock(key, cacheMap::get);
        if (value != null) {
            return (T) value;
        }

        try {
            T loadedValue = valueLoader.call();
            put(key, loadedValue);
            return loadedValue;
        } catch (Exception e) {
            throw new WorkflowRuntimeException(e);
        }
    }

    public Collection<Object> getAllValues() {
        if (isOrderedOrHasLimitedSize()) {
            return applyFunctionInsideOptimisticReadBlock(null, item -> {
                final var result = new ArrayList<>(cacheMap.size());
                keys.forEach(key -> find(key).ifPresent(result::add));
                return result;
            });
        } else {
            return applyFunctionInsideOptimisticReadBlock(null, item -> cacheMap.values());
        }
    }

    @Override
    public void put(Object key, Object value) {
        acceptConsumerInsideWriteBlock(key, item -> {
            if (isOrderedOrHasLimitedSize()) {
                find(item).ifPresentOrElse(oldItem -> {
                    removeFromTheEnd(item);
                    if (oldItem != value) {
                        cacheMap.put(item, value);
                    }
                }, () -> cacheMap.put(item, value));
                keys.addFirst(item);
                if (cacheMap.size() > maxSize) {
                    removeFromTheEnd(keys.getLast());
                }
            } else {
                cacheMap.put(item, value);
            }
        });
    }

    @Override
    public void evict(Object key) {
        acceptConsumerInsideWriteBlock(key, item -> {
            if (isOrderedOrHasLimitedSize()) {
                removeFromTheEnd(item);
            }
            cacheMap.remove(item);
        });
    }

    @Override
    public void clear() {
        acceptConsumerInsideWriteBlock(null, item -> {
            if (isOrderedOrHasLimitedSize()) {
                keys.clear();
            }
            cacheMap.clear();
        });
    }

    public int size() {
        return applyFunctionInsideOptimisticReadBlock(null, item -> cacheMap.size());
    }

    private boolean isOrderedOrHasLimitedSize() {
        return ordered || maxSize < Integer.MAX_VALUE;
    }

    private Optional<Object> find(Object key) {
        return ofNullable(cacheMap.get(key));
    }

    private void removeFromTheEnd(Object key) {
        final var iterator = keys.listIterator(keys.size());
        while (iterator.hasPrevious()) {
            final var element = iterator.previous();
            if (element.equals(key)) {
                iterator.remove();
                break;
            }
        }
    }

    private <K, V> V applyFunctionInsideOptimisticReadBlock(K key, Function<K, V> function) {
        var stamp = stampedLock.tryOptimisticRead();
        V value = function.apply(key);
        if (!stampedLock.validate(stamp)) {
            stamp = stampedLock.readLock();
            try {
                value = function.apply(key);
            } finally {
                stampedLock.unlockRead(stamp);
            }
        }
        return value;
    }

    private <K> void acceptConsumerInsideWriteBlock(K key, Consumer<K> consumer) {
        var stamp = stampedLock.writeLock();
        try {
            consumer.accept(key);
        } finally {
            stampedLock.unlockWrite(stamp);
        }
    }

}
