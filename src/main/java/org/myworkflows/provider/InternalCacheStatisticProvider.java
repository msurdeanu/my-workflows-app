package org.myworkflows.provider;

import lombok.RequiredArgsConstructor;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.StatisticItem;
import org.myworkflows.domain.StatisticItemGroup;
import org.springframework.stereotype.Component;

import java.util.stream.Collectors;

import static java.lang.String.format;
import static java.lang.String.valueOf;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Component
@RequiredArgsConstructor
public final class InternalCacheStatisticProvider implements StatisticProvider {

    private final InternalCacheManager cacheManager;

    @Override
    public StatisticItemGroup getStatisticItemGroup() {
        return StatisticItemGroup.builder()
            .root(StatisticItem.builder()
                .name("statistics.internal-caches.group")
                .icon("vaadin:folder-o")
                .build())
            .leafs(cacheManager.getCacheNames().stream()
                .map(cacheManager::getCache)
                .filter(cache -> cache instanceof InternalCache)
                .map(InternalCache.class::cast)
                .map(cacheName -> StatisticItem.builder()
                    .name("statistics.internal-caches.group." + cacheName.getName() + ".name")
                    .icon("vaadin:file-o")
                    .value(cacheToStats(cacheName))
                    .description("statistics.internal-caches.group." + cacheName.getName() + ".description")
                    .build())
                .collect(Collectors.toList()))
            .build();
    }

    private String cacheToStats(InternalCache cache) {
        return format("(%d, %s, %b)", cache.size(),
            cache.getMaxSize() == Integer.MAX_VALUE ? "∞" : valueOf(cache.getMaxSize()), cache.isOrdered());
    }

}
