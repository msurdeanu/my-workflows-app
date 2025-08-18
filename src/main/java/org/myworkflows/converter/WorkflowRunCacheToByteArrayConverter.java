package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.holder.file.BinaryFileSource;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import static org.myworkflows.holder.file.FileSourceHolder.INSTANCE;
import static org.myworkflows.util.ByteArrayCompressUtil.compress;
import static org.myworkflows.util.ByteArrayCompressUtil.decompress;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@Converter
public final class WorkflowRunCacheToByteArrayConverter implements AttributeConverter<WorkflowRunCache, Byte[]> {

    private static final UuidToByteArrayConverter CONVERTER = new UuidToByteArrayConverter();

    @Override
    public Byte[] convertToDatabaseColumn(WorkflowRunCache attribute) {
        final var uuid = attribute.getId();

        try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
             ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
            objectOutputStream.writeObject(attribute);
            INSTANCE.writeToSource(BinaryFileSource.of(uuid.toString()), compress(byteArrayOutputStream.toByteArray()));
        } catch (IOException exception) {
            log.warn("Could not write workflow run cache to file", exception);
        }

        return CONVERTER.convertToDatabaseColumn(uuid);
    }

    @Override
    public WorkflowRunCache convertToEntityAttribute(Byte[] data) {
        final var uuid = CONVERTER.convertToEntityAttribute(data);

        try (ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(decompress(
            INSTANCE.readFromSource(BinaryFileSource.of(uuid.toString()))));
             ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream)) {
            return (WorkflowRunCache) objectInputStream.readObject();
        } catch (Exception exception) {
            log.warn("Could not read workflow run cache from file", exception);
            return new WorkflowRunCache(uuid);
        }
    }

}
