package org.myworkflows.provider;

import lombok.RequiredArgsConstructor;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.domain.StatisticItem;
import org.myworkflows.domain.StatisticItemGroup;
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
public class InternalCacheStatisticProvider implements StatisticProvider {

    private final List<InternalCache<?, ?>> internalCaches;

    @Override
    public StatisticItemGroup getStatisticItemGroup() {
        return StatisticItemGroup.builder()
            .root(StatisticItem.builder()
                .name("statistics.internal-caches.group")
                .icon("vaadin:folder-o")
                .build())
            .leafs(internalCaches.stream()
                .map(internalCache -> StatisticItem.builder()
                    .name("statistics.internal-caches.group." + internalCache.getName() + ".name")
                    .icon("vaadin:file-o")
                    .value(cacheToStats(internalCache))
                    .description("statistics.internal-caches.group." + internalCache.getName() + ".description")
                    .build())
                .collect(Collectors.toList()))
            .build();
    }

    private String cacheToStats(InternalCache<?, ?> internalCache) {
        final var cacheStats = internalCache.stats();
        return format("(%.4f, %d, %d)", cacheStats.hitRate(), cacheStats.totalLoadTime(), internalCache.estimatedSize());
    }

}
