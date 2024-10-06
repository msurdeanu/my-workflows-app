package org.myworkflows.cache;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class InternalCacheTest {

    @Test
    public void testNonOrderedCache() {
        // given
        final var cache = new InternalCache("test", 1, false);

        // when & then
        assertEquals("test", cache.getName());
        assertEquals(0, cache.size());
        assertNull(cache.get("key"));
        cache.put("key", "value");
        assertEquals("value", cache.get("key", String.class));
        assertEquals(1, cache.size());
        cache.put("key1", "value1");
        assertEquals(2, cache.size());
        cache.evict("key");
        assertInstanceOf(String.class, cache.get("key1", String.class));
        cache.clear();
        assertEquals(0, cache.size());
    }

    @Test
    public void testOrderedCache() {
        // given
        final var cache = new InternalCache("test", 3, true);

        // when & then
        cache.put("key1", "value1");
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

}
