package org.myworkflows.view.component.html;

import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldVariant;
import com.vaadin.flow.data.value.ValueChangeMode;

import java.util.function.Consumer;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class TextFieldWithEnterShortcut extends TextField {

    public TextFieldWithEnterShortcut(Consumer<String> valueConsumer) {
        setWidthFull();
        setSuffixComponent(VaadinIcon.ENTER.create());
        setValueChangeMode(ValueChangeMode.EAGER);
        addValueChangeListener(event -> {
            ofNullable(event.getValue())
                .filter(item -> item.trim().isEmpty())
                .ifPresentOrElse(item -> {
                    setInvalid(true);
                    setErrorMessage(getTranslation("field.empty.error"));
                }, () -> setInvalid(false));
        });
        addShortcutListener(this, () -> {
            final var value = getValue().trim();
            if (!value.isEmpty()) {
                valueConsumer.accept(value);
            }
        }, Key.ENTER);
    }

    public TextFieldWithEnterShortcut allowedCharPattern(String allowedCharPattern) {
        setAllowedCharPattern(allowedCharPattern);
        return this;
    }

    public TextFieldWithEnterShortcut placeholder(String placeholder) {
        setPlaceholder(placeholder);
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
