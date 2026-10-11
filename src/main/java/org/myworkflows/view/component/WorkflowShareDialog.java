package org.myworkflows.view.component;

import com.vaadin.flow.component.ComponentEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.shared.Registration;
import lombok.Getter;

/**
 * Shows the link of a workflow definition before it is shared, together with what the link carries. The dialog does not
 * touch the clipboard by itself: it fires a {@link CopyEvent} with the link and the listener decides what to do with it.
 *
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class WorkflowShareDialog extends Dialog {

    private final TextArea linkField = new TextArea(getTranslation("dialog.share.link.label"));
    private final Checkbox readOnlyCheckbox = new Checkbox(getTranslation("dialog.share.read-only.label"), true);
    private final Checkbox parametersCheckbox = new Checkbox(getTranslation("dialog.share.parameters.label"), true);

    private final LinkFactory linkFactory;

    public WorkflowShareDialog(String definitionName, LinkFactory linkFactory, boolean hasParameters, boolean unsavedChanges) {
        this.linkFactory = linkFactory;

        addClassName("workflow-share-dialog");
        setHeaderTitle(getTranslation("dialog.share.header", definitionName));
        setWidth("min(560px, 96vw)");

        linkField.setReadOnly(true);
        linkField.setWidthFull();
        readOnlyCheckbox.addValueChangeListener(_ -> refreshLink());
        parametersCheckbox.addValueChangeListener(_ -> refreshLink());
        parametersCheckbox.setVisible(hasParameters);
        refreshLink();

        final var layout = new VerticalLayout(linkField, readOnlyCheckbox, parametersCheckbox);
        layout.setPadding(false);
        if (unsavedChanges) {
            final var unsavedChangesMessage = new Div(getTranslation("dialog.share.unsaved.message"));
            unsavedChangesMessage.addClassNames("workflow-status", "running");
            layout.add(unsavedChangesMessage);
        }
        add(layout);

        final var closeButton = new Button(getTranslation("dialog.share.button.close"), _ -> close());
        closeButton.addThemeVariants(ButtonVariant.TERTIARY);
        final var copyButton = new Button(getTranslation("dialog.share.button.copy"), VaadinIcon.LINK.create(), _ -> copy());
        copyButton.addThemeVariants(ButtonVariant.PRIMARY);
        copyButton.setAutofocus(true);
        getFooter().add(closeButton, copyButton);
    }

    public Registration addCopyListener(ComponentEventListener<CopyEvent> listener) {
        return addListener(CopyEvent.class, listener);
    }

    private void refreshLink() {
        linkField.setValue(linkFactory.create(readOnlyCheckbox.getValue(), parametersCheckbox.isVisible() && parametersCheckbox.getValue()));
    }

    private void copy() {
        close();
        fireEvent(new CopyEvent(this, linkField.getValue()));
    }

    /**
     * @author Mihai Surdeanu
     * @since 1.3
     */
    @FunctionalInterface
    public interface LinkFactory {

        String create(boolean readOnly, boolean withParameters);

    }

    /**
     * @author Mihai Surdeanu
     * @since 1.3
     */
    @Getter
    public static final class CopyEvent extends ComponentEvent<WorkflowShareDialog> {

        private final String link;

        public CopyEvent(WorkflowShareDialog source, String link) {
            super(source, false);
            this.link = link;
        }

    }

}
