package org.myworkflows.config;

import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCache.InternalCacheConfig;
import org.myworkflows.domain.Placeholder;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import static java.time.Duration.ofSeconds;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Configuration("cacheConfig")
@EnableCaching
public class CacheConfig {

    @Primary
    @Bean("workflowRunCache")
    public InternalCache<Object, Object> workflowRunCache() {
        return new InternalCache<>("workflowRunCache",
            InternalCacheConfig.builder().ordered(true).maxSize(1_000).expireAfterWrite(ofSeconds(86_400)).build());
    }

    @Bean("workflowTemplateCache")
    public InternalCache<Object, Object> workflowTemplateCache() {
        return new InternalCache<>("workflowTemplateCache",
            InternalCacheConfig.builder().maxSize(1_000).expireAfterWrite(ofSeconds(0)).build());
    }

    @Bean("workflowDefinitionCache")
    public InternalCache<Object, Object> workflowDefinitionCache() {
        return new InternalCache<>("workflowDefinitionCache",
            InternalCacheConfig.builder().maxSize(10_000).expireAfterWrite(ofSeconds(0)).build());
    }

    @Bean("menuItemCache")
    public InternalCache<Object, Object> menuItemCache() {
        return new InternalCache<>("menuItemCache",
            InternalCacheConfig.builder().maxSize(0).expireAfterWrite(ofSeconds(86_400)).build());
    }

    @Bean("placeholderCache")
    public InternalCache<String, Placeholder> placeholderCache() {
        return new InternalCache<>("placeholderCache",
            InternalCacheConfig.builder().maxSize(0).expireAfterWrite(ofSeconds(86_400)).build());
    }

}
