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
public final class MenuItemPathToStringConverter implements AttributeConverter<MenuItemPath<?>, String> {

    private static final String CLASS_PREFIX = "class://";

    @Override
    public String convertToDatabaseColumn(MenuItemPath<?> attribute) {
        return ofNullable(attribute)
            .map(MenuItemPath::value)
            .map(value -> value instanceof String attributePath ? attributePath : CLASS_PREFIX + value.getClass().getName())
            .orElse(null);
    }

    @Override
    public MenuItemPath<?> convertToEntityAttribute(String path) {
        return ofNullable(path)
            .map(item -> item.startsWith(CLASS_PREFIX) ? findMenuItemViewPath(item.replace(CLASS_PREFIX, StringUtils.EMPTY)) : new MenuItemPath<>(item))
            .orElse(null);
    }

    private MenuItemPath<Class<? extends Component>> findMenuItemViewPath(String path) {
        try {
            final var targetClass = Class.forName(path);
            return Component.class.isAssignableFrom(targetClass) ? new MenuItemPath<>((Class<? extends Component>) targetClass) : null;
        } catch (ClassNotFoundException notUsed) {
            // if class is not found, the target component will be null and will not appear in the final menu
        }

        return null;
    }

}
