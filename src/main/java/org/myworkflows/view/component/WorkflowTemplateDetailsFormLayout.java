package org.myworkflows.view.component;

import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextAreaVariant;
import com.vaadin.flow.data.value.ValueChangeMode;
import org.myworkflows.domain.ExecutionPrint;

import java.util.ArrayList;
import java.util.Map;

import static java.util.Optional.ofNullable;
import static org.myworkflows.domain.ExecutionPrint.NULL_AS_STR;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowTemplateDetailsFormLayout extends FormLayout {

    private static final int TEXTAREA_CHAR_LIMIT = 32_768;

    public WorkflowTemplateDetailsFormLayout() {
        setResponsiveSteps(new ResponsiveStep("0", 2));
    }

    public void setExecutionPrint(final ExecutionPrint executionPrint) {
        removeAll();

        final var textAreas = new ArrayList<TextArea>();
        if (executionPrint.value() instanceof Map<?, ?> executionPrintAsMap) {
            executionPrintAsMap.forEach((key, value) -> textAreas.add(createTextArea(key.toString(),
                ofNullable(value).map(item -> item.getClass().getSimpleName().toLowerCase()).orElse(NULL_AS_STR),
                ofNullable(value).map(Object::toString).orElse(NULL_AS_STR))));
        } else {
            textAreas.add(createTextArea(executionPrint.name(), executionPrint.type(), executionPrint.fullValue()));
        }
        textAreas.forEach(textArea -> {
            textArea.setReadOnly(true);
            textArea.addThemeVariants(TextAreaVariant.LUMO_SMALL);
            textArea.setMaxHeight("200px");
            textArea.setValueChangeMode(ValueChangeMode.EAGER);
            textArea.addValueChangeListener(event -> event.getSource().setHelperText(event.getValue().length() + "/" + TEXTAREA_CHAR_LIMIT));
            add(textArea);
        });

        if (textAreas.size() == 1) {
            setColspan(textAreas.get(0), 2);
        }
    }

    private TextArea createTextArea(final String name, final String type, final String value) {
        final var textArea = new TextArea();
        textArea.setTooltipText(getTranslation("workflow-print.grid.details.tooltip", type, name));
        textArea.setValue(value);
        textArea.focus();
        return textArea;
    }

}
