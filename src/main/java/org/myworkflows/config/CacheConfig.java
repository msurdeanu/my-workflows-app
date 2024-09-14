package org.myworkflows.config;

import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import java.time.Duration;
import java.util.List;

import static java.time.Duration.ofSeconds;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Configuration("cacheConfig")
@EnableCaching
public class CacheConfig {

    @Primary
    @Bean("workflowRunCacheManager")
    public CacheManager workflowRunCacheManager() {
        final var cacheManager = new CustomCacheManager(1_000, ofSeconds(0), ofSeconds(86_400));
        cacheManager.setCacheNames(List.of("workflow-runs"));
        return cacheManager;
    }

    @Bean("workflowTemplateCacheManager")
    public CacheManager workflowTemplateCacheManager() {
        final var cacheManager = new CustomCacheManager(10_000, ofSeconds(0), ofSeconds(0));
        cacheManager.setCacheNames(List.of("workflow-templates"));
        return cacheManager;
    }

    @Bean("workflowDefinitionCacheManager")
    public CacheManager workflowDefinitionCacheManager() {
        final var cacheManager = new CustomCacheManager(10_000, ofSeconds(0), ofSeconds(0));
        cacheManager.setCacheNames(List.of("workflow-definitions"));
        return cacheManager;
    }

    @Bean("menuItemCacheManager")
    public CacheManager menuItemCacheManager() {
        final var cacheManager = new CustomCacheManager(0, ofSeconds(0), ofSeconds(86_400));
        cacheManager.setCacheNames(List.of("menu-items"));
        return cacheManager;
    }

    @Bean("placeholderCacheManager")
    public CacheManager placeholderCacheManager() {
        final var cacheManager = new CustomCacheManager(0, ofSeconds(0), ofSeconds(86_400));
        cacheManager.setCacheNames(List.of("placeholders"));
        return cacheManager;
    }

    private static class CustomCacheManager extends CaffeineCacheManager {

        public CustomCacheManager(long maxSize, Duration expireAfterAccess, Duration expireAfterWrite) {
            final var caffeine = Caffeine.newBuilder()
                .recordStats();

            if (maxSize > 0) {
                caffeine.maximumSize(maxSize);
            }

            if (expireAfterAccess.getSeconds() > 0) {
                caffeine.expireAfterAccess(expireAfterAccess);
            }

            if (expireAfterWrite.getSeconds() > 0) {
                caffeine.expireAfterWrite(expireAfterWrite);
            }

            setCaffeine(caffeine);
        }

    }

}
