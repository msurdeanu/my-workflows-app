package org.myworkflows.view.component.html;

import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldVariant;

import java.util.function.Consumer;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class TextFieldWithEnterShortcut extends TextField {

    public TextFieldWithEnterShortcut(Consumer<String> valueConsumer) {
        setWidthFull();
        setSuffixComponent(VaadinIcon.ENTER.create());
        addShortcutListener(this, () -> valueConsumer.accept(getValue()), Key.ENTER);
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

}
