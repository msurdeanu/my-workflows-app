package org.myworkflows.converter;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunCache;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunCacheToByteArrayConverterTest {

    @Test
    public void testWorkflowRunCacheCompleteSerialization() {
        final var workflowRunCache = new WorkflowRunCache();
        workflowRunCache.put("a", "z");
        workflowRunCache.put("b", 1);

        final var workflowRunCacheToByteArrayConverter = new WorkflowRunCacheToByteArrayConverter();
        final var workflowRunCacheAsBytes = workflowRunCacheToByteArrayConverter.convertToDatabaseColumn(workflowRunCache);
        final var deserializedWorkflowRunCache = workflowRunCacheToByteArrayConverter.convertToEntityAttribute(workflowRunCacheAsBytes);
        assertNotNull(deserializedWorkflowRunCache);
        assertEquals("z", deserializedWorkflowRunCache.get("a"));
        assertEquals(1, deserializedWorkflowRunCache.get("b"));
        assertTrue(deserializedWorkflowRunCache.isCacheObjectMapComplete());
    }

    @Test
    public void testWorkflowRunCacheIncompleteSerialization() {
        final var workflowRunCache = new WorkflowRunCache();
        workflowRunCache.put("a", new MyClassWhichIsNotSerializable(1));

        final var workflowRunCacheToByteArrayConverter = new WorkflowRunCacheToByteArrayConverter();
        final var workflowRunCacheAsBytes = workflowRunCacheToByteArrayConverter.convertToDatabaseColumn(workflowRunCache);
        final var deserializedWorkflowRunCache = workflowRunCacheToByteArrayConverter.convertToEntityAttribute(workflowRunCacheAsBytes);
        assertNotNull(deserializedWorkflowRunCache);
        assertNull(deserializedWorkflowRunCache.get("a"));
        assertFalse(deserializedWorkflowRunCache.isCacheObjectMapComplete());
    }

    public record MyClassWhichIsNotSerializable(int value) {

    }

}
