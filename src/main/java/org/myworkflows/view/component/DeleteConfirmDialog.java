package org.myworkflows.view.component;

import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.confirmdialog.ConfirmDialog;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class DeleteConfirmDialog extends ConfirmDialog {

    public DeleteConfirmDialog(String itemName, ComponentEventListener<ConfirmEvent> listener) {
        setHeader(getTranslation("dialog.confirm.delete.header", itemName));
        setText(getTranslation("dialog.confirm.delete.text"));
        setCancelable(true);
        setConfirmText(getTranslation("dialog.confirm.delete.button.confirm"));
        setConfirmButtonTheme("error primary");
        addConfirmListener(listener);
    }

}
