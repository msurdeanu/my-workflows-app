package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.domain.WorkflowRunCache;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.util.ByteArrayUtil;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import static org.myworkflows.util.ByteArrayUtil.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Converter
public final class ExecutionCacheToByteArrayConverter implements AttributeConverter<WorkflowRunCache, Byte[]> {

    @Override
    public Byte[] convertToDatabaseColumn(WorkflowRunCache attribute) {
        try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
             ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
            objectOutputStream.writeObject(attribute);
            return toObject(byteArrayOutputStream.toByteArray());
        } catch (IOException e) {
            throw new WorkflowRuntimeException(e);
        }
    }

    @Override
    public WorkflowRunCache convertToEntityAttribute(Byte[] data) {
        try (ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(ByteArrayUtil.toPrimitive(data));
             ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream)) {
            return (WorkflowRunCache) objectInputStream.readObject();
        } catch (IOException | ClassNotFoundException e) {
            throw new WorkflowRuntimeException(e);
        }
    }

}
