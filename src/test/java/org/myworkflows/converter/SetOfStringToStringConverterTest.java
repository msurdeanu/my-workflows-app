package org.myworkflows.converter;

import org.junit.jupiter.api.Test;

import java.util.LinkedHashSet;
import java.util.Set;

import static java.lang.String.join;
import static java.util.Collections.addAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SetOfStringToStringConverterTest {

    @Test
    public void testConvertToDatabaseColumn() {
        final var setOfStringToStringConverter = new SetOfStringToStringConverter();

        assertNull(setOfStringToStringConverter.convertToDatabaseColumn(null));
        assertEquals("1,2,3", setOfStringToStringConverter.convertToDatabaseColumn(createSet()));
    }

    @Test
    public void testConvertToEntityAttribute() {
        final var setOfStringToStringConverter = new SetOfStringToStringConverter();

        assertNull(setOfStringToStringConverter.convertToEntityAttribute(null));
        assertEquals("123", join("", setOfStringToStringConverter.convertToEntityAttribute("1,2,3")));
    }

    @Test
    public void whenDatabaseColumnIsEmptyThenAnEmptySetIsReturned() {
        final var setOfStringToStringConverter = new SetOfStringToStringConverter();

        assertTrue(setOfStringToStringConverter.convertToEntityAttribute("").isEmpty());
        assertEquals(Set.of("1"), setOfStringToStringConverter.convertToEntityAttribute("1,,"));
    }

    private Set<String> createSet() {
        final var hashSet = new LinkedHashSet<String>();
        addAll(hashSet, "1", "2", "3");
        return hashSet;
    }

}
