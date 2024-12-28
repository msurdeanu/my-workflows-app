package org.myworkflows.view.component;

import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextAreaVariant;
import com.vaadin.flow.data.value.ValueChangeMode;
import org.myworkflows.domain.WorkflowRunPrint;

import java.util.ArrayList;
import java.util.Map;

import static java.util.Optional.ofNullable;
import static org.myworkflows.domain.WorkflowRunPrint.NULL_AS_STR;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowPrintDetailsFormLayout extends FormLayout {

    private static final int TEXTAREA_CHAR_LIMIT = 32_768;
    private static final int COLUMNS = 2;

    public WorkflowPrintDetailsFormLayout() {
        setResponsiveSteps(new ResponsiveStep("0", COLUMNS));
    }

    public void setExecutionPrint(WorkflowRunPrint workflowRunPrint) {
        removeAll();

        final var textAreas = new ArrayList<TextArea>();
        if (workflowRunPrint.value() instanceof Map<?, ?> executionPrintAsMap) {
            executionPrintAsMap.forEach((key, value) -> textAreas.add(createTextArea(key.toString(),
                ofNullable(value).map(item -> item.getClass().getSimpleName().toLowerCase()).orElse(NULL_AS_STR),
                ofNullable(value).map(Object::toString).orElse(NULL_AS_STR))));
        } else {
            textAreas.add(createTextArea(workflowRunPrint.name(), workflowRunPrint.type(), workflowRunPrint.fullValue()));
        }
        textAreas.forEach(textArea -> {
            textArea.setReadOnly(true);
            textArea.addThemeVariants(TextAreaVariant.LUMO_SMALL);
            textArea.setMaxHeight("200px");
            textArea.setValueChangeMode(ValueChangeMode.EAGER);
            textArea.addValueChangeListener(event -> event.getSource().setHelperText(event.getValue().length() + "/" + TEXTAREA_CHAR_LIMIT));
            add(textArea);
        });

        final var textAreasSize = textAreas.size();
        if (textAreasSize % 2 == 1) {
            setColspan(textAreas.get(textAreasSize - 1), COLUMNS);
        }
    }

    private TextArea createTextArea(String name, String type, String value) {
        final var textArea = new TextArea();
        textArea.setTooltipText(getTranslation("workflow-print.grid.details.tooltip", type, name));
        textArea.setValue(value);
        textArea.focus();
        return textArea;
    }

}
