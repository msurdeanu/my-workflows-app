package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.domain.SettingType;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
@Converter
public class SettingTypeToStringConverter implements AttributeConverter<SettingType, String> {

    @Override
    public String convertToDatabaseColumn(SettingType attribute) {
        return ofNullable(attribute).map(SettingType::getValue).orElse(null);
    }

    @Override
    public SettingType convertToEntityAttribute(String data) {
        return ofNullable(data).map(SettingType::of).orElse(null);
    }

}
