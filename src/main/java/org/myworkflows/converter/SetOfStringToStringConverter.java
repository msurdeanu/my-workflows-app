package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.apache.commons.lang3.StringUtils;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import static java.lang.String.join;
import static java.util.Arrays.stream;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Converter
public final class SetOfStringToStringConverter implements AttributeConverter<Set<String>, String> {

    private static final String COMMA_DELIMITER = ",";

    @Override
    public String convertToDatabaseColumn(Set<String> attribute) {
        return ofNullable(attribute)
            .map(item -> join(COMMA_DELIMITER, item))
            .orElse(null);
    }

    @Override
    public Set<String> convertToEntityAttribute(String data) {
        return ofNullable(data)
            // An empty column would otherwise be split into a single, empty entry.
            .map(item -> stream(item.split(COMMA_DELIMITER))
                .filter(StringUtils::isNotEmpty)
                .collect(Collectors.toCollection(LinkedHashSet::new)))
            .orElse(null);
    }

}
