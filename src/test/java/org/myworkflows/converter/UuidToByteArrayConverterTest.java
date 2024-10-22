package org.myworkflows.converter;

import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class UuidToByteArrayConverterTest {

    @Test
    public void whenConverterIsUsedEverythingWorksAsExpected() {
        // given
        final var initialUuid = UUID.randomUUID();
        final var converter = new UuidToByteArrayConverter();

        // when & then
        final var convertedUuid = converter.convertToEntityAttribute(converter.convertToDatabaseColumn(initialUuid));
        assertEquals(initialUuid, convertedUuid);
    }

}
