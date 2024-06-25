package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.domain.ParameterType;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Converter
public class ParameterTypeToStringConverter implements AttributeConverter<ParameterType, String> {

    @Override
    public String convertToDatabaseColumn(ParameterType attribute) {
        return ofNullable(attribute).map(ParameterType::getValue).orElse(null);
    }

    @Override
    public ParameterType convertToEntityAttribute(String data) {
        return ofNullable(data).map(ParameterType::of).orElse(null);
    }

}
