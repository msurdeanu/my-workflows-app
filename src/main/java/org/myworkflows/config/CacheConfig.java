package org.myworkflows.config;

import lombok.Getter;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCache.InternalCacheConfig;
import org.myworkflows.domain.Placeholder;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import static java.time.Duration.ofSeconds;

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

    @Primary
    @Bean("workflowRunCache")
    public InternalCache<Object, Object> workflowRunCache() {
        return new InternalCache<>("workflowRunCache",
                InternalCacheConfig.builder().ordered(true).maxSize(workflowRunMaxSize)
                        .expireAfterWrite(ofSeconds(workflowRunExpireAfterWriteSeconds)).build());
    }

    @Bean("workflowTemplateCache")
    public InternalCache<Object, Object> workflowTemplateCache() {
        return new InternalCache<>("workflowTemplateCache",
                InternalCacheConfig.builder().maxSize(0).expireAfterWrite(ofSeconds(0)).build());
    }

    @Bean("workflowDefinitionCache")
    public InternalCache<Object, Object> workflowDefinitionCache() {
        return new InternalCache<>("workflowDefinitionCache",
                InternalCacheConfig.builder().maxSize(0).expireAfterWrite(ofSeconds(0)).build());
    }

    @Bean("menuItemCache")
    public InternalCache<Object, Object> menuItemCache() {
        return new InternalCache<>("menuItemCache",
                InternalCacheConfig.builder().maxSize(0).expireAfterWrite(ofSeconds(0)).build());
    }

    @Bean("placeholderCache")
    public InternalCache<String, Placeholder> placeholderCache() {
        return new InternalCache<>("placeholderCache",
                InternalCacheConfig.builder().maxSize(0).expireAfterWrite(ofSeconds(0)).build());
    }

}
