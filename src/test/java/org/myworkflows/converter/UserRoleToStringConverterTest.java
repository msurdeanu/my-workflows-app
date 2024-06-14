package org.myworkflows.converter;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.UserRole;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class UserRoleToStringConverterTest {

    @Test
    public void testConvertToDatabaseColumn() {
        final var userRoleToStringConverter = new UserRoleToStringConverter();

        assertNull(userRoleToStringConverter.convertToDatabaseColumn(null));
        assertEquals("ROLE_ADMIN", userRoleToStringConverter.convertToDatabaseColumn(UserRole.ADMIN));
        assertEquals("ROLE_GUEST", userRoleToStringConverter.convertToDatabaseColumn(UserRole.GUEST));
    }

    @Test
    public void testConvertToEntityAttribute() {
        final var userRoleToStringConverter = new UserRoleToStringConverter();

        assertEquals(UserRole.GUEST, userRoleToStringConverter.convertToEntityAttribute(null));
        assertEquals(UserRole.LOGGED, userRoleToStringConverter.convertToEntityAttribute("ROLE_LOGGED"));
        assertEquals(UserRole.GUEST, userRoleToStringConverter.convertToEntityAttribute("Role_Logged"));
    }

}
