package org.myworkflows.view.component.html;

import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldVariant;
import com.vaadin.flow.data.value.ValueChangeMode;

import java.util.function.Consumer;

import static java.util.Optional.ofNullable;
import static org.myworkflows.view.util.KeyboardUtil.addKeydownEventListener;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class TextFieldWithEnterShortcut extends TextField {

    public TextFieldWithEnterShortcut(Consumer<String> valueConsumer) {
        setWidthFull();
        setSuffixComponent(VaadinIcon.ENTER.create());
        setValueChangeMode(ValueChangeMode.EAGER);
        addValueChangeListener(event -> ofNullable(event.getValue())
            .filter(item -> item.trim().isEmpty())
            .ifPresentOrElse(item -> {
                setInvalid(true);
                setErrorMessage(getTranslation("field.empty.error"));
            }, () -> setInvalid(false)));
        addKeydownEventListener(getElement(), event -> {
            final var value = getValue().trim();
            if (!value.isEmpty()) {
                valueConsumer.accept(value);
            }
        }, "Enter");
    }

    public TextFieldWithEnterShortcut allowedCharPatternAndPlaceholder(String regexPattern) {
        setAllowedCharPattern(regexPattern);
        setPlaceholder(regexPattern);
        return this;
    }

    public TextFieldWithEnterShortcut small() {
        addThemeVariants(TextFieldVariant.LUMO_SMALL);
        return this;
    }

    public TextFieldWithEnterShortcut width(String width) {
        setWidth(width);
        return this;
    }

}
