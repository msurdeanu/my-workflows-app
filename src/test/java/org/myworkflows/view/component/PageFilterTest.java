package org.myworkflows.view.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.TextField;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class PageFilterTest {

    @Test
    public void testTextFilterIsInUseWhenNotBlank() {
        // given
        final var field = new TextField();
        final var filter = PageFilter.text("name", field, value -> "Name: " + value);

        // then
        assertEquals("", filter.getter().get());
        field.setValue("  backup ");
        assertEquals("backup", filter.getter().get());
        filter.setter().accept("");
        assertEquals("", field.getValue());
        assertEquals("Name: backup", filter.describer().apply("backup"));
    }

    @Test
    public void testChoiceFilterLeavesTheDefaultOutOfTheUrl() {
        // given
        final var field = new Select<Breakpoint>();
        field.setItems(Breakpoint.values());
        field.setValue(Breakpoint.DESKTOP);
        final var filter = PageFilter.choice("tier", field, Breakpoint.DESKTOP, Breakpoint::name,
            PageFilterTest::breakpointOf, breakpoint -> "Tier " + breakpoint.name());

        // then
        assertEquals("", filter.getter().get());
        filter.setter().accept("PHONE");
        assertEquals(Breakpoint.PHONE, field.getValue());
        assertEquals("PHONE", filter.getter().get());
        assertEquals("Tier PHONE", filter.describer().apply("PHONE"));
    }

    @Test
    public void testChoiceFilterFallsBackToTheDefaultOnAnUnknownValue() {
        // given
        final var field = new Select<Breakpoint>();
        field.setItems(Breakpoint.values());
        field.setValue(Breakpoint.PHONE);
        final var filter = PageFilter.choice("tier", field, Breakpoint.DESKTOP, Breakpoint::name,
            PageFilterTest::breakpointOf, Breakpoint::name);

        // then
        filter.setter().accept("bogus");
        assertEquals(Breakpoint.DESKTOP, field.getValue());
        assertNull(filter.describer().apply("bogus"));
        filter.setter().accept("TABLET");
        filter.setter().accept("");
        assertEquals(Breakpoint.DESKTOP, field.getValue());
    }

    @Test
    public void testChoiceFilterWithoutADefaultValueIsInUseOnceAValueIsPicked() {
        // given
        final var field = new Select<Breakpoint>();
        field.setItems(Breakpoint.values());
        field.setEmptySelectionAllowed(true);
        final var filter = PageFilter.choice("tier", field, null, Breakpoint::name, PageFilterTest::breakpointOf, Breakpoint::name);

        // then
        assertEquals("", filter.getter().get());
        filter.setter().accept("TABLET");
        assertEquals("TABLET", filter.getter().get());
        filter.setter().accept("");
        assertNull(field.getValue());
    }

    @Test
    public void testChoiceFilterOfACheckboxIsInUseWhileTicked() {
        // given
        final var field = new Checkbox();
        final var filter = PageFilter.choice("mine", field, false, _ -> "true",
            value -> Boolean.parseBoolean(value) ? true : null, _ -> "Only my runs");

        // then
        assertEquals("", filter.getter().get());
        filter.setter().accept("true");
        assertTrue(field.getValue());
        assertEquals("true", filter.getter().get());
        assertEquals("Only my runs", filter.describer().apply("true"));
        filter.setter().accept("yes");
        assertFalse(field.getValue());
    }

    private static Breakpoint breakpointOf(String name) {
        return Arrays.stream(Breakpoint.values()).filter(breakpoint -> breakpoint.name().equals(name)).findFirst().orElse(null);
    }

}
