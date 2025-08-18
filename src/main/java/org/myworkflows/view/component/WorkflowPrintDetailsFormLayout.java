package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextAreaVariant;
import com.vaadin.flow.server.streams.DownloadResponse;
import org.myworkflows.domain.WorkflowRunPrint;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Map;

import static com.vaadin.flow.server.streams.DownloadHandler.fromInputStream;
import static java.util.Optional.ofNullable;
import static org.myworkflows.domain.WorkflowRunPrint.NULL_AS_STR;

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

    public void setExecutionPrint(WorkflowRunPrint workflowRunPrint) {
        removeAll();

        final var components = new ArrayList<Component>();
        if (workflowRunPrint.value() instanceof Map<?, ?> executionPrintAsMap) {
            executionPrintAsMap.forEach((key, value) -> components.add(createComponent(key.toString(),
                ofNullable(value).map(item -> item.getClass().getSimpleName().toLowerCase()).orElse(NULL_AS_STR),
                ofNullable(value).map(Object::toString).orElse(NULL_AS_STR))));
        } else {
            components.add(createComponent(workflowRunPrint.name(), workflowRunPrint.type(), workflowRunPrint.fullValue()));
        }

        components.forEach(this::add);
        final var componentSize = components.size();
        if (componentSize % 2 == 1) {
            setColspan(components.get(componentSize - 1), COLUMNS);
        }
    }

    private Component createComponent(String name, String type, String value) {
        if (value.length() > TEXTAREA_CHAR_LIMIT) {
            final var data = value.getBytes();
            return new Anchor(fromInputStream(event -> {
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
            textArea.focus();
            return textArea;
        }
    }

}
