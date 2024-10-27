package org.myworkflows.cache;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.util.Arrays;
import java.util.Collection;
import java.util.EnumMap;
import java.util.Map;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
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

    public void addCache(CacheNameEnum cacheNameEnum, int maxSize, boolean ordered) {
        caches.put(cacheNameEnum, new InternalCache(cacheNameEnum.getName(), maxSize, ordered));
    }

    @Getter
    @RequiredArgsConstructor
    public enum CacheNameEnum {
        WORKFLOW_PARAMETER("workflowParameter"),
        WORKFLOW_DEFINITION("workflowDefinition"),
        WORKFLOW_TEMPLATE("workflowTemplate"),
        WORKFLOW_RUN("workflowRun"),
        MENU_ITEM("menuItem"),
        PLACEHOLDER("placeholder"),
        DOC_PAGE("docPage"),
        DEFAULT("default");

        private final String name;

        public static CacheNameEnum of(String name) {
            return Arrays.stream(CacheNameEnum.values())
                .filter(cacheName -> cacheName.name.equals(name))
                .findFirst().orElse(DEFAULT);
        }
    }

}
