package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.exception.WorkflowDuplicateException.Reason;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Predicate;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;

/**
 * Names the copy of a definition or a template before it is made. A name which is taken is refused while it is typed,
 * and once more by the service, which may know of a copy made in the meantime: the dialog stays open then.
 *
 * @author Mihai Surdeanu
 * @since 2.2
 */
public final class WorkflowDuplicateDialog extends Dialog {

    private final TextField nameField = new TextField(getTranslation("dialog.duplicate.name.label"));
    private final Button duplicateButton = new Button(getTranslation("dialog.duplicate.button.duplicate"), VaadinIcon.COPY_O.create());
    private final VerticalLayout layout = new VerticalLayout(nameField);

    private final Predicate<String> nameTaken;
    private final Consumer<String> duplicateAction;

    /**
     * @param nameTaken whether a name, without the blanks around it, is the one of an item of the same kind.
     * @param duplicateAction makes the copy under the given name.
     */
    public WorkflowDuplicateDialog(String originalName, String suggestedName, Predicate<String> nameTaken, Consumer<String> duplicateAction) {
        this.nameTaken = nameTaken;
        this.duplicateAction = duplicateAction;

        addClassName("workflow-duplicate-dialog");
        setHeaderTitle(getTranslation("dialog.duplicate.header", originalName));
        setWidth("min(560px, 96vw)");

        nameField.setValue(suggestedName);
        nameField.setWidthFull();
        nameField.setAutofocus(true);
        nameField.setAutoselect(true);
        nameField.setManualValidation(true);
        nameField.setValueChangeMode(ValueChangeMode.EAGER);
        nameField.addValueChangeListener(_ -> validate());
        addShortcutListener(nameField, this::duplicate, Key.ENTER);
        layout.setPadding(false);
        layout.addClassName("workflow-duplicate-content");
        add(layout);

        final var cancelButton = new Button(getTranslation("dialog.duplicate.button.cancel"), _ -> close());
        cancelButton.addThemeVariants(ButtonVariant.TERTIARY);
        duplicateButton.addThemeVariants(ButtonVariant.PRIMARY);
        duplicateButton.addClickListener(_ -> duplicate());
        getFooter().add(cancelButton, duplicateButton);
    }

    /**
     * Adds what the copy takes from the original, under the name.
     */
    public void addDetails(Component... components) {
        layout.add(components);
    }

    private boolean validate() {
        final var name = nameField.getValue().trim();
        final Optional<String> error;
        if (name.isEmpty()) {
            error = Optional.of(describe(Reason.NAME_MISSING, name));
        } else if (nameTaken.test(name)) {
            error = Optional.of(describe(Reason.NAME_TAKEN, name));
        } else {
            error = Optional.empty();
        }
        showError(error.orElse(null));
        return error.isEmpty();
    }

    private void duplicate() {
        if (!validate()) {
            return;
        }
        try {
            duplicateAction.accept(nameField.getValue());
            close();
        } catch (WorkflowDuplicateException exception) {
            final var message = describe(exception.getReason(), exception.getName());
            if (exception.getReason() != Reason.ORIGINAL_CHANGED) {
                showError(message);
                return;
            }
            close();
            Notification.show(message, 10_000, Notification.Position.BOTTOM_START).addThemeVariants(NotificationVariant.ERROR);
        }
    }

    private void showError(String message) {
        nameField.setInvalid(message != null);
        nameField.setErrorMessage(message);
        duplicateButton.setEnabled(message == null);
    }

    private String describe(Reason reason, String name) {
        return getTranslation(reason.getTranslationKey(), name);
    }

}
