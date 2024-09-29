package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.util.ByteArrayUtil;

import java.nio.ByteBuffer;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Converter
public final class UuidToByteArrayConverter implements AttributeConverter<UUID, Byte[]> {

    @Override
    public Byte[] convertToDatabaseColumn(UUID attribute) {
        final var byteBuffer = ByteBuffer.wrap(new byte[16]);
        byteBuffer.putLong(attribute.getMostSignificantBits());
        byteBuffer.putLong(attribute.getLeastSignificantBits());
        return ByteArrayUtil.toObject(byteBuffer.array());
    }

    @Override
    public UUID convertToEntityAttribute(Byte[] data) {
        final var byteBuffer = ByteBuffer.wrap(ByteArrayUtil.toPrimitive(data));
        final var high = byteBuffer.getLong();
        final var low = byteBuffer.getLong();
        return new UUID(high, low);
    }

}
