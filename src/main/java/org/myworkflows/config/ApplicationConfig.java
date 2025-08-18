package org.myworkflows.config;

import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.provider.DatabaseSettingProvider;
import org.myworkflows.provider.DefaultSettingProvider;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.SettingRepository;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.lang.reflect.Proxy;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@EnableCaching
@Configuration("applicationConfig")
public class ApplicationConfig {

    @Bean
    public SettingProvider settingProvider(SettingRepository settingRepository) {
        return (SettingProvider) Proxy.newProxyInstance(getClass().getClassLoader(), new Class[]{SettingProvider.class},
            new DatabaseSettingProvider(new DefaultSettingProvider(), settingRepository));
    }

    @Bean
    public InternalCacheManager cacheManager(SettingProvider settingProvider) {
        final var cacheManager = new InternalCacheManager();
        cacheManager.addCache(CacheNameEnum.WORKFLOW_RUN, settingProvider.getOrDefault("workflowRunMaxSize", 250), InternalCache.InternalCacheOrder.LIFO);
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

