package org.myworkflows.config;

import lombok.Getter;
import org.myworkflows.cache.InternalCacheManager;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import static java.time.Duration.ofSeconds;
import static org.myworkflows.cache.InternalCache.InternalCacheConfig;
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
        cacheManager.addCache(CacheNameEnum.WORKFLOW_RUN, InternalCacheConfig.builder()
            .ordered(true).maxSize(workflowRunMaxSize).expireAfterWrite(ofSeconds(workflowRunExpireAfterWriteSeconds))
            .build());
        cacheManager.addCache(CacheNameEnum.WORKFLOW_TEMPLATE, InternalCacheConfig.builder()
            .maxSize(0).expireAfterWrite(ofSeconds(0))
            .build());
        cacheManager.addCache(CacheNameEnum.WORKFLOW_DEFINITION, InternalCacheConfig.builder()
            .maxSize(0).expireAfterWrite(ofSeconds(0))
            .build());
        cacheManager.addCache(CacheNameEnum.WORKFLOW_PARAMETER, InternalCacheConfig.builder()
            .maxSize(0).expireAfterWrite(ofSeconds(0))
            .build());
        cacheManager.addCache(CacheNameEnum.MENU_ITEM, InternalCacheConfig.builder()
            .maxSize(0).expireAfterWrite(ofSeconds(0))
            .build());
        cacheManager.addCache(CacheNameEnum.PLACEHOLDER, InternalCacheConfig.builder()
            .maxSize(0).expireAfterWrite(ofSeconds(0))
            .build());
        return cacheManager;
    }

}
