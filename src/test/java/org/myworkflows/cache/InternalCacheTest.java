package org.myworkflows.cache;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;

public class InternalCacheTest {

    @Test
    public void testNonOrderedCache() {
        // given
        final var cache = new InternalCache("test", 1);

        // when & then
        assertEquals("test", cache.getName());
        assertInstanceOf(HashMap.class, cache.getNativeCache());
        assertEquals(0, cache.size());
        assertNull(cache.get("key"));
        cache.put("key", "value");
        assertEquals("value", cache.get("key", String.class));
        assertEquals(1, cache.size());
        cache.put("key1", "value1");
        assertEquals(2, cache.size());
        cache.evict("key");
        assertInstanceOf(String.class, cache.get("key1", String.class));
        assertEquals(1, cache.getAllValues().size());
        cache.clear();
        assertEquals(0, cache.size());
    }

    @Test
    public void testReverseOrderedCache() {
        // given
        final var cache = new InternalCache("test", 3, InternalCache.InternalCacheOrder.REVERSE);

        // when & then
        cache.get("key1", () -> "value1");
        cache.get("key1", () -> "value2");
        cache.put("key2", "value2");
        cache.put("key3", "value3");
        assertEquals(3, cache.size());
        final var values = cache.getAllValues().toArray();
        assertEquals(3, values.length);
        assertEquals("value3", values[0]);
        assertEquals("value2", values[1]);
        assertEquals("value1", values[2]);
        cache.put("key2", "other");
        assertEquals(3, cache.size());
        final var newValues = cache.getAllValues().toArray();
        assertEquals("other", newValues[0]);
        assertEquals("value3", newValues[1]);
        assertEquals("value1", newValues[2]);
    }

    @Test
    public void testUnlimitedCache() {
        // given
        final var cache = new InternalCache("test");

        // when
        IntStream.range(0, 10_000).forEach(index -> cache.put(index, "value" + index));

        // then
        assertEquals(10_000, cache.size());
        assertEquals(10_000, cache.getAllValues().size());
        assertEquals(Integer.MAX_VALUE, cache.getMaxSize());
    }

}
