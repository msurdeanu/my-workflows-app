package org.myworkflows.config;

import com.github.benmanes.caffeine.cache.Caffeine;
import jakarta.validation.constraints.NotNull;
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

    @Bean
    @Primary
    public CacheManager placeholderCacheManager() {
        final var cacheManager = new CustomCacheManager(0, ofSeconds(0), ofSeconds(86_400));
        cacheManager.setCacheNames(List.of("placeholders"));
        return cacheManager;
    }

    private static class CustomCacheManager extends CaffeineCacheManager {

        public CustomCacheManager(final long maxSize,
                                  @NotNull final Duration expireAfterAccess,
                                  @NotNull final Duration expireAfterWrite) {
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
