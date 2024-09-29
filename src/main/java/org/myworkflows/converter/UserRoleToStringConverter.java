package org.myworkflows.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.myworkflows.domain.UserRole;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Converter
public final class UserRoleToStringConverter implements AttributeConverter<UserRole, String> {

    @Override
    public String convertToDatabaseColumn(UserRole attribute) {
        return ofNullable(attribute).map(UserRole::getLabel).orElse(null);
    }

    @Override
    public UserRole convertToEntityAttribute(String data) {
        return ofNullable(data).map(UserRole::of).orElse(UserRole.GUEST);
    }

}
