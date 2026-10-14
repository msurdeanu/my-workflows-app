package org.myworkflows.view.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import org.myworkflows.domain.UserTemporaryPassword;
import org.myworkflows.view.util.ClipboardUtil;

/**
 * Shows the temporary password of a user to the admin who asked for it, the one time it can be shown: only its hash is
 * saved. The dialog is therefore closed by its button alone, not by a click next to it or by Escape.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class UserPasswordDialog extends Dialog {

    public UserPasswordDialog(UserTemporaryPassword temporaryPassword) {
        setHeaderTitle(getTranslation("dialog.user-password.header", temporaryPassword.user().getUsername()));
        setWidth("min(480px, 96vw)");
        setCloseOnOutsideClick(false);
        setCloseOnEsc(false);

        final var passwordField = new TextField(getTranslation("dialog.user-password.password.label"));
        passwordField.addClassName("user-password");
        passwordField.setReadOnly(true);
        passwordField.setValue(temporaryPassword.password());
        passwordField.setWidthFull();

        final var layout = new VerticalLayout(new Div(getTranslation("dialog.user-password.text")), passwordField);
        layout.setPadding(false);
        add(layout);

        final var closeButton = new Button(getTranslation("dialog.user-password.button.close"), _ -> close());
        closeButton.addThemeVariants(ButtonVariant.TERTIARY);
        final var copyButton = new Button(getTranslation("dialog.user-password.button.copy"), VaadinIcon.COPY.create(), _ -> {
            ClipboardUtil.copyTo(getElement(), temporaryPassword.password());
            Notification.show(getTranslation("dialog.user-password.copied.message"));
        });
        copyButton.addThemeVariants(ButtonVariant.PRIMARY);
        copyButton.setAutofocus(true);
        getFooter().add(closeButton, copyButton);
    }

}
