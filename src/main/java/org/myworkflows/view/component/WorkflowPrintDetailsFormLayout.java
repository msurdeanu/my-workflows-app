package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextAreaVariant;
import com.vaadin.flow.server.streams.DownloadResponse;
import org.myworkflows.domain.WorkflowRunVariable;

import java.io.ByteArrayInputStream;

import static com.vaadin.flow.server.streams.DownloadHandler.fromInputStream;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class WorkflowPrintDetailsFormLayout extends FormLayout {

    private static final int TEXTAREA_CHAR_LIMIT = 16_384;
    private static final int COLUMNS = 2;

    public WorkflowPrintDetailsFormLayout() {
        setResponsiveSteps(new ResponsiveStep("0", COLUMNS));
    }

    /**
     * Shows a value which was written down when somebody looked at what a run held: it is a text by now, whatever it
     * was then.
     */
    public void setVariable(WorkflowRunVariable workflowRunVariable) {
        setText(workflowRunVariable.name(), workflowRunVariable.type(), workflowRunVariable.value());
    }

    /**
     * Shows a value as the text it reads like. A text which is too long to be read here is offered as a download.
     */
    public void setText(String name, String type, String value) {
        removeAll();

        final var component = createComponent(name, type, value);
        add(component);
        setColspan(component, COLUMNS);
    }

    private Component createComponent(String name, String type, String value) {
        if (value.length() > TEXTAREA_CHAR_LIMIT) {
            final var data = value.getBytes();
            return new Anchor(fromInputStream(_ -> {
                try {
                    return new DownloadResponse(new ByteArrayInputStream(data), "value.txt", "text/plain", data.length);
                } catch (Exception notUsed) {
                    return DownloadResponse.error(500);
                }
            }), getTranslation("workflow-print.grid.details.download"));
        } else {
            final var textArea = new TextArea();
            textArea.setTooltipText(getTranslation("workflow-print.grid.details.tooltip", type, name));
            textArea.setValue(value);
            textArea.setReadOnly(true);
            textArea.addThemeVariants(TextAreaVariant.LUMO_SMALL);
            textArea.setMaxHeight("200px");
            // scroll to the latest output without focusing, as focus draws the read-only focus ring
            textArea.addAttachListener(_ -> textArea.getElement()
                .executeJs("requestAnimationFrame(() => this.scrollToEnd())"));
            return textArea;
        }
    }

}
