package org.myworkflows.cache;

import org.junit.jupiter.api.Test;
import org.myworkflows.cache.InternalCache.InternalCacheConfig;

import static java.util.concurrent.TimeUnit.SECONDS;
import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class InternalCacheTest {

    @Test
    public void testNonOrderedCache() {
        // given
        final var cache = new InternalCache("test",
            InternalCacheConfig.builder().maxSize(1).build());

        // when & then
        assertEquals("test", cache.getName());
        assertEquals(0, cache.estimatedSize());
        assertNull(cache.get("key"));
        cache.put("key", "value");
        assertEquals("value", cache.get("key", String.class));
        assertEquals(1, cache.estimatedSize());
        cache.put("key1", "value1");
        await().atMost(5, SECONDS).until(() -> 1 == cache.estimatedSize());
        assertTrue(cache.find("key").isEmpty());
        assertInstanceOf(String.class, cache.get("key1", String.class));
        cache.evict("key1");
        assertEquals(0, cache.estimatedSize());
        final var stats = cache.stats();
        assertEquals(2L, stats.hitCount());
        assertEquals(2L, stats.missCount());
    }

    @Test
    public void testOrderedCache() {
        // given
        final var cache = new InternalCache("test", InternalCacheConfig.builder().maxSize(3).ordered(true).build());

        // when & then
        cache.put("key1", "value1");
        cache.put("key2", "value2");
        cache.put("key3", "value3");
        assertEquals(3, cache.estimatedSize());
        final var values = cache.getAllValues().toArray();
        assertEquals(3, values.length);
        assertEquals("value3", values[0]);
        assertEquals("value2", values[1]);
        assertEquals("value1", values[2]);
        cache.put("key2", "other");
        await().atMost(5, SECONDS).until(() -> 3 == cache.estimatedSize());
        final var newValues = cache.getAllValues().toArray();
        assertEquals("other", newValues[0]);
        assertEquals("value3", newValues[1]);
        assertEquals("value1", newValues[2]);
        final var stats = cache.stats();
        assertEquals(7L, stats.hitCount()); // 2 x 3 items returned by getAllValues + 1 override
        assertEquals(3L, stats.missCount()); // due to initial puts
    }

}
