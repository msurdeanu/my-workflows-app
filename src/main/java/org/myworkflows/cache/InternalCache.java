package org.myworkflows.cache;

import lombok.Getter;
import lombok.NonNull;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.springframework.cache.support.SimpleValueWrapper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.locks.StampedLock;
import java.util.function.Consumer;
import java.util.function.Function;

import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toCollection;
import static java.util.stream.Collectors.toSet;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class InternalCache implements org.springframework.cache.Cache {

    private final String name;

    @Getter
    private final int maxSize;

    @Getter
    private final InternalCacheOrder cacheOrder;

    private final Map<Object, Object> cacheMap = new HashMap<>();

    private final LinkedList<Object> keys = new LinkedList<>();

    private final StampedLock stampedLock = new StampedLock();

    public InternalCache(String name) {
        this(name, Integer.MAX_VALUE, InternalCacheOrder.NO);
    }

    public InternalCache(String name, int maxSize) {
        this(name, maxSize, InternalCacheOrder.NO);
    }

    public InternalCache(String name, int maxSize, InternalCacheOrder cacheOrder) {
        this.name = name;
        this.maxSize = Integer.max(1, maxSize);
        this.cacheOrder = cacheOrder;
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
    public <T> T get(@NonNull Object key, @NonNull Callable<T> valueLoader) {
        return ofNullable(applyFunctionInsideOptimisticReadBlock(key, cacheMap::get))
            .map(item -> (T) item)
            .orElseGet(() -> {
                try {
                    T loadedValue = valueLoader.call();
                    put(key, loadedValue);
                    return loadedValue;
                } catch (Exception e) {
                    throw new WorkflowRuntimeException(e);
                }
            });
    }

    public <T> Set<T> getAllKeys(Class<T> type) {
        assert type != null;
        if (isOrderedOrHasLimitedSize()) {
            return applyFunctionInsideOptimisticReadBlock(null,
                item -> keys.stream().filter(type::isInstance).map(type::cast).collect(toCollection(LinkedHashSet::new)));
        } else {
            return applyFunctionInsideOptimisticReadBlock(null,
                item -> cacheMap.keySet().stream().filter(type::isInstance).map(type::cast).collect(toSet()));
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
    public void put(@NonNull Object key, Object value) {
        acceptConsumerInsideWriteBlock(key, item -> {
            if (ofNullable(cacheMap.put(item, value)).isEmpty() && isOrderedOrHasLimitedSize()) {
                if (cacheOrder == InternalCacheOrder.FIFO) {
                    keys.addLast(item);
                } else {
                    keys.addFirst(item);
                }
                if (cacheMap.size() > maxSize) {
                    cacheMap.remove(removeItemFromTheList());
                }
            }
        });
    }

    @Override
    public void evict(@NonNull Object key) {
        acceptConsumerInsideWriteBlock(key, item -> {
            if (isOrderedOrHasLimitedSize()) {
                keys.removeFirstOccurrence(item);
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
        return cacheOrder != InternalCacheOrder.NO || maxSize < Integer.MAX_VALUE;
    }

    private Optional<Object> find(Object key) {
        return ofNullable(cacheMap.get(key));
    }

    private Object removeItemFromTheList() {
        return cacheOrder == InternalCacheOrder.FIFO ? keys.removeFirst() : keys.removeLast();
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

    public enum InternalCacheOrder {
        NO, FIFO, LIFO
    }

}
