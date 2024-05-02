package org.myworkflows.provider;

import com.github.benmanes.caffeine.cache.Cache;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.tuple.Pair;
import org.myworkflows.domain.StatisticItem;
import org.myworkflows.domain.StatisticItemGroup;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

import static java.lang.String.format;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Component
@RequiredArgsConstructor
public class CacheManagerStatisticProvider implements StatisticProvider {

    private final List<CacheManager> cacheManagers;

    @Override
    public StatisticItemGroup getStatisticItemGroup() {
        return StatisticItemGroup.builder()
                .root(StatisticItem.builder()
                        .name("statistics.internal-caches.group")
                        .icon("vaadin:folder-o")
                        .build())
                .leafs(cacheManagers.stream()
                        .flatMap(
                                cacheManager -> cacheManager.getCacheNames().stream().map(cacheName -> Pair.of(cacheName, cacheManager.getCache(cacheName))))
                        .filter(pairOfCache -> pairOfCache.getRight() instanceof CaffeineCache)
                        .map(pairOfCache -> StatisticItem.builder()
                                .name("statistics.internal-caches.group." + pairOfCache.getLeft() + ".name")
                                .icon("vaadin:file-o")
                                .value(cacheToString(((CaffeineCache) pairOfCache.getRight()).getNativeCache()))
                                .description("statistics.internal-caches.group." + pairOfCache.getLeft() + ".description")
                                .build())
                        .collect(Collectors.toList()))
                .build();
    }

    private String cacheToString(Cache<Object, Object> cache) {
        final var cacheStats = cache.stats();
        return format("(%.4f, %d, %d)", cacheStats.hitRate(), cacheStats.totalLoadTime(), cache.estimatedSize());
    }

}
