package org.myworkflows.view.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import org.myworkflows.domain.UserTokenSecret;
import org.myworkflows.view.util.ClipboardUtil;

/**
 * Shows an API token to the user who has just created it, the one time it can be shown: only its digest is saved. The
 * dialog is therefore closed by its button alone, not by a click next to it or by Escape.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class UserTokenDialog extends Dialog {

    public UserTokenDialog(UserTokenSecret tokenSecret) {
        setHeaderTitle(getTranslation("dialog.user-token.header", tokenSecret.userToken().getName()));
        // wide enough for the 64 characters of a token to be read at once
        setWidth("min(680px, 96vw)");
        setCloseOnOutsideClick(false);
        setCloseOnEsc(false);

        final var tokenField = new TextField(getTranslation("dialog.user-token.token.label"));
        tokenField.addClassName("user-password");
        tokenField.setReadOnly(true);
        tokenField.setValue(tokenSecret.secret());
        tokenField.setWidthFull();

        final var layout = new VerticalLayout(new Div(getTranslation("dialog.user-token.text")), tokenField);
        layout.setPadding(false);
        add(layout);

        final var closeButton = new Button(getTranslation("dialog.user-token.button.close"), _ -> close());
        closeButton.addThemeVariants(ButtonVariant.TERTIARY);
        final var copyButton = new Button(getTranslation("dialog.user-token.button.copy"), VaadinIcon.COPY.create(), _ -> {
            ClipboardUtil.copyTo(getElement(), tokenSecret.secret());
            Notification.show(getTranslation("dialog.user-token.copied.message"));
        });
        copyButton.addThemeVariants(ButtonVariant.PRIMARY);
        copyButton.setAutofocus(true);
        getFooter().add(closeButton, copyButton);
    }

}
