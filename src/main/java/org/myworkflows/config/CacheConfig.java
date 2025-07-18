package org.myworkflows.config;

import lombok.Getter;
import lombok.Setter;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Setter
@Configuration("cacheConfig")
@ConfigurationProperties(prefix = "my-workflows.config.cache")
@EnableCaching
public class CacheConfig {

    private int workflowRunMaxSize = 250;

    @Bean
    public InternalCacheManager cacheManager() {
        final var cacheManager = new InternalCacheManager();
        cacheManager.addCache(CacheNameEnum.WORKFLOW_RUN, workflowRunMaxSize, InternalCache.InternalCacheOrder.LIFO);
        cacheManager.addCache(CacheNameEnum.WORKFLOW_TEMPLATE, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.LIFO);
        cacheManager.addCache(CacheNameEnum.WORKFLOW_DEFINITION, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.LIFO);
        cacheManager.addCache(CacheNameEnum.WORKFLOW_PARAMETER, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.LIFO);
        cacheManager.addCache(CacheNameEnum.WORKFLOW_PLACEHOLDER, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.LIFO);
        cacheManager.addCache(CacheNameEnum.MENU_ITEM, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.NO);
        cacheManager.addCache(CacheNameEnum.LIBRARY, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.NO);
        cacheManager.addCache(CacheNameEnum.DOC_PAGE, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.FIFO);
        return cacheManager;
    }

}
