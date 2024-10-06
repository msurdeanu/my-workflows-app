package org.myworkflows.config;

import lombok.Getter;
import org.myworkflows.cache.InternalCacheManager;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import static org.myworkflows.cache.InternalCacheManager.CacheNameEnum;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Configuration("cacheConfig")
@ConfigurationProperties(prefix = "my-workflows.config.cache")
@EnableCaching
public class CacheConfig {

    private final int workflowRunMaxSize = 1_000;
    private final long workflowRunExpireAfterWriteSeconds = 86_400;

    @Bean
    public InternalCacheManager cacheManager() {
        final var cacheManager = new InternalCacheManager();
        cacheManager.addCache(CacheNameEnum.WORKFLOW_RUN, workflowRunMaxSize, true);
        cacheManager.addCache(CacheNameEnum.WORKFLOW_TEMPLATE, Integer.MAX_VALUE, true);
        cacheManager.addCache(CacheNameEnum.WORKFLOW_DEFINITION, Integer.MAX_VALUE, true);
        cacheManager.addCache(CacheNameEnum.WORKFLOW_PARAMETER, Integer.MAX_VALUE, true);
        cacheManager.addCache(CacheNameEnum.MENU_ITEM, Integer.MAX_VALUE, false);
        cacheManager.addCache(CacheNameEnum.PLACEHOLDER, Integer.MAX_VALUE, false);
        return cacheManager;
    }

}
