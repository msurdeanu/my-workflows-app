package org.myworkflows.converter;

import com.vaadin.flow.component.Component;
import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.MenuItemPath;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Converter
public class MenuItemPathToStringConverter implements AttributeConverter<MenuItemPath<?>, String> {

    private static final String CLASS_PREFIX = "class://";

    @Override
    public String convertToDatabaseColumn(MenuItemPath<?> attribute) {
        if (attribute == null) {
            return null;
        }

        final var value = attribute.getValue();
        if (value instanceof String attributePath) {
            return attributePath;
        }

        return ofNullable(value)
            .map(item -> CLASS_PREFIX + item.getClass().getName())
            .orElse(null);
    }

    @Override
    public MenuItemPath<?> convertToEntityAttribute(String path) {
        if (path == null) {
            return null;
        }

        if (path.startsWith(CLASS_PREFIX)) {
            return findMenuItemViewPath(path.replace(CLASS_PREFIX, StringUtils.EMPTY));
        }

        return new MenuItemPath<>(path);
    }

    private MenuItemPath<Class<? extends Component>> findMenuItemViewPath(String path) {
        try {
            final var targetClass = Class.forName(path);
            return Component.class.isAssignableFrom(targetClass)
                ? new MenuItemPath<>((Class<? extends Component>) targetClass)
                : null;
        } catch (ClassNotFoundException notUsed) {
            // if class is not found, the target component will be null and will not appear in the final menu
        }

        return null;
    }

}
