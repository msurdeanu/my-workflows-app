package org.myworkflows.provider;

import org.junit.jupiter.api.Test;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_RUN;
import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_TEMPLATE;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class InternalCacheStatisticProviderTest {

    @Test
    public void whenCacheStatisticProviderIsCalledWithTwoCachesThenEverythingWorksAsExpected() {
        // given
        final var internalCacheManager = new InternalCacheManager();
        internalCacheManager.addCache(WORKFLOW_RUN, 100, InternalCache.InternalCacheOrder.LIFO);
        internalCacheManager.addCache(WORKFLOW_TEMPLATE, Integer.MAX_VALUE, InternalCache.InternalCacheOrder.NO);

        // when & then
        final var internalCacheStatisticProvider = new InternalCacheStatisticProvider(internalCacheManager);
        assertNotNull(internalCacheStatisticProvider);
        final var statisticItemGroup = internalCacheStatisticProvider.getStatisticItemGroup();
        assertNotNull(statisticItemGroup);
        assertEquals(2, statisticItemGroup.getLeafs().size());
        assertEquals("statistics.internal-caches.group.workflowTemplate.name", statisticItemGroup.getLeafs().getFirst().getName());
        assertEquals("(0, ∞, NO)", statisticItemGroup.getLeafs().getFirst().getValue());
        assertEquals("statistics.internal-caches.group.workflowRun.name", statisticItemGroup.getLeafs().getLast().getName());
        assertEquals("(0, 100, LIFO)", statisticItemGroup.getLeafs().getLast().getValue());
    }

}
