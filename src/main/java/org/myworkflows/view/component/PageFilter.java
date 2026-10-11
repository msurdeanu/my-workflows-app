package org.myworkflows.view.component;

import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.textfield.TextField;
import jakarta.annotation.Nullable;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.util.Optional.ofNullable;

/**
 * One filter of a page, kept in the URL of the page as a query parameter (see {@link PageFilters}).
 *
 * @param name the name of the query parameter
 * @param source the field holding the filter, listened to for changes; {@code null} for a filter set only from the URL
 * @param getter the current value, as the query parameter spells it; empty when the filter is not in use
 * @param setter applies a value spelled as the query parameter; an empty one brings the filter back to its default
 * @param describer the text of the chip shown while the filter is in use, or {@code null} for a filter without a chip
 * @author Mihai Surdeanu
 * @since 2.0
 */
public record PageFilter(String name, @Nullable HasValue<?, ?> source, Supplier<String> getter, Consumer<String> setter,
                         Function<String, String> describer) {

    /**
     * @return a filter typed into a text field; it is in use as soon as the field is not blank.
     */
    public static PageFilter text(String name, TextField field, Function<String, String> describer) {
        return new PageFilter(name, field, () -> field.getValue().strip(), field::setValue, describer);
    }

    /**
     * A filter picked from a list of values is in use while its value is not the default one; a parameter naming no
     * known value brings it back to the default.
     *
     * @return the filter kept by the given field.
     */
    public static <T> PageFilter choice(String name, HasValue<?, T> field, T defaultValue, Function<T, String> toParameter,
                                        Function<String, T> fromParameter, Function<T, String> describer) {
        return new PageFilter(name, field,
            () -> field.getValue() == null || Objects.equals(field.getValue(), defaultValue) ? StringUtils.EMPTY : toParameter.apply(field.getValue()),
            value -> field.setValue(value.isEmpty() ? defaultValue : ofNullable(fromParameter.apply(value)).orElse(defaultValue)),
            value -> ofNullable(fromParameter.apply(value)).map(describer).orElse(null));
    }

}
