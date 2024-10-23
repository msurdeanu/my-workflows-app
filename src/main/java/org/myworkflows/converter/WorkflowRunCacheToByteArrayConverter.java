package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.util.ByteArrayCompressUtil;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.zip.DataFormatException;

import static org.myworkflows.util.ByteArrayCompressUtil.compress;
import static org.myworkflows.util.ByteArrayUtil.toObject;
import static org.myworkflows.util.ByteArrayUtil.toPrimitive;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Converter
public final class WorkflowRunCacheToByteArrayConverter implements AttributeConverter<WorkflowRunCache, Byte[]> {

    @Override
    public Byte[] convertToDatabaseColumn(WorkflowRunCache attribute) {
        try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
             ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
            objectOutputStream.writeObject(attribute);
            return compressAndLog(byteArrayOutputStream, attribute);
        } catch (IOException exception) {
            throw new WorkflowRuntimeException(exception);
        }
    }

    @Override
    public WorkflowRunCache convertToEntityAttribute(Byte[] data) {
        try (ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(ByteArrayCompressUtil.decompress(toPrimitive(data)));
             ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream)) {
            return (WorkflowRunCache) objectInputStream.readObject();
        } catch (IOException | ClassNotFoundException | DataFormatException exception) {
            throw new WorkflowRuntimeException(exception);
        }
    }

    private Byte[] compressAndLog(ByteArrayOutputStream byteArrayOutputStream, WorkflowRunCache workflowRunCache) {
        final var initialByteArray = byteArrayOutputStream.toByteArray();
        final var compressedByteArray = compress(initialByteArray);
        if (log.isDebugEnabled()) {
            log.debug("Workflow run cache with keys '{}', will be saved in database with {}% memory reduction by using compression.",
                workflowRunCache.getAllKeys(), (initialByteArray.length - compressedByteArray.length) * 100 / initialByteArray.length);
        }
        return toObject(compressedByteArray);
    }

}
