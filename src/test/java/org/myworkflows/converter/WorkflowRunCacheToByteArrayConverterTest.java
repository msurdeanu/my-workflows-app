package org.myworkflows.converter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunCache;

import java.io.IOException;
import java.nio.file.Files;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.myworkflows.holder.file.FileSourceHolder.INSTANCE;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunCacheToByteArrayConverterTest {

    @BeforeEach
    public void setUp() throws IOException {
        final var path = Files.createTempDirectory("files");
        INSTANCE.setBaseDirectory(path.toAbsolutePath().toString());
    }

    @Test
    public void testWorkflowRunCacheCompleteSerialization() {
        final var workflowRunCache = new WorkflowRunCache(UUID.randomUUID());
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
        final var workflowRunCache = new WorkflowRunCache(UUID.randomUUID());
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
