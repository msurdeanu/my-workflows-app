package org.myworkflows.converter;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.MenuItemPath;
import org.myworkflows.view.WorkflowRunView;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Mihai Surdeanu
 * @since 1.2.0
 */
public final class MenuItemPathToStringConverterTest {

    @Test
    public void testConvertToDatabaseColumn() {
        final var menuItemPathToStringConverter = new MenuItemPathToStringConverter();

        // when null, return null
        assertNull(menuItemPathToStringConverter.convertToDatabaseColumn(null));
        // when value is string, return string
        assertEquals("test", menuItemPathToStringConverter.convertToDatabaseColumn(new MenuItemPath<>("test")));
        // when value is class, return class
        assertEquals("class://java.lang.Class", menuItemPathToStringConverter.convertToDatabaseColumn(new MenuItemPath<>(WorkflowRunView.class)));
    }

    @Test
    public void testConvertToEntityAttribute() {
        final var menuItemPathToStringConverter = new MenuItemPathToStringConverter();

        // when null, return null
        assertNull(menuItemPathToStringConverter.convertToEntityAttribute(null));
        // when value is string, return string
        final var menuItemPathAsString = menuItemPathToStringConverter.convertToEntityAttribute("test");
        assertNotNull(menuItemPathAsString);
        assertEquals("test", menuItemPathAsString.value());
        // when value is class inherited from Component, return class as string
        final var menuItemPathAsClass = menuItemPathToStringConverter.convertToEntityAttribute("class://org.myworkflows.view.WorkflowRunView");
        assertNotNull(menuItemPathAsClass);
        assertEquals(WorkflowRunView.class, menuItemPathAsClass.value());
        // when value is class but not inherited from Component, return null
        assertNull(menuItemPathToStringConverter.convertToEntityAttribute("class://org.myworkflows.EventBroadcaster"));
        // when value is not found class, return null
        assertNull(menuItemPathToStringConverter.convertToEntityAttribute("class://org.myworkflows.bla.BlaView"));
    }

}
