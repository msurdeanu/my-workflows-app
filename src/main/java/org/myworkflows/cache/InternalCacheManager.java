package org.myworkflows.cache;

import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.util.Collection;
import java.util.EnumMap;
import java.util.Map;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class InternalCacheManager implements CacheManager {

    private final Map<CacheNameEnum, Cache> caches = new EnumMap<>(CacheNameEnum.class);

    @Override
    public Cache getCache(String name) {
        return caches.get(CacheNameEnum.of(name));
    }

    @Override
    public Collection<String> getCacheNames() {
        return caches.keySet().stream().map(CacheNameEnum::getName).toList();
    }

    public void addCache(CacheNameEnum cacheNameEnum, int maxSize, InternalCache.InternalCacheOrder cacheOrder) {
        caches.put(cacheNameEnum, new InternalCache(cacheNameEnum.getName(), maxSize, cacheOrder));
    }

}
