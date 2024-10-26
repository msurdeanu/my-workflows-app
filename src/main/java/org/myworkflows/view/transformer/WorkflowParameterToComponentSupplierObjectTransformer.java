package org.myworkflows.view.transformer;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextField;
import lombok.Builder;
import lombok.Getter;
import org.myworkflows.domain.WorkflowParameter;

import java.time.LocalDate;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;

import static java.lang.String.valueOf;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowParameterToComponentSupplierObjectTransformer
    implements Transformer<WorkflowParameter, WorkflowParameterToComponentSupplierObjectTransformer.ComponentSupplierObject> {

    @Override
    public ComponentSupplierObject transform(WorkflowParameter workflowParameter) {
        final var workflowParameterType = workflowParameter.getType();
        return switch (workflowParameterType) {
            case DATE -> createDateField(workflowParameter);
            case PASS -> createPasswordField(workflowParameter);
            case INT -> createIntegerField(workflowParameter);
            case DOUBLE -> createNumberField(workflowParameter);
            case BOOL -> createCheckboxField(workflowParameter);
            case S_STR -> createStringSelect(workflowParameter);
            default -> createTextField(workflowParameter);
        };
    }

    private static ComponentSupplierObject createDateField(WorkflowParameter workflowParameter) {
        final var datePicker = new DatePicker();
        datePicker.setValue((LocalDate) workflowParameter.getComputedValue());
        datePicker.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(datePicker)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(datePicker::getValue)
                .valueAsStringSupplier(() -> datePicker.getValue().toString())
                .build())
            .build();
    }

    private static ComponentSupplierObject createPasswordField(WorkflowParameter workflowParameter) {
        final var passwordField = new PasswordField();
        passwordField.setValue((String) workflowParameter.getComputedValue());
        passwordField.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(passwordField)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(passwordField::getValue)
                .valueAsStringSupplier(passwordField::getValue)
                .build())
            .build();
    }

    private static ComponentSupplierObject createIntegerField(WorkflowParameter workflowParameter) {
        final var integerField = new IntegerField();
        integerField.setValue((Integer) workflowParameter.getComputedValue());
        integerField.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(integerField)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(integerField::getValue)
                .valueAsStringSupplier(() -> valueOf(integerField.getValue()))
                .build())
            .build();
    }

    private static ComponentSupplierObject createNumberField(WorkflowParameter workflowParameter) {
        final var numberField = new NumberField();
        numberField.setValue((Double) workflowParameter.getComputedValue());
        numberField.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(numberField)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(numberField::getValue)
                .valueAsStringSupplier(() -> valueOf(numberField.getValue()))
                .build())
            .build();
    }

    private static ComponentSupplierObject createCheckboxField(WorkflowParameter workflowParameter) {
        final var checkbox = new Checkbox();
        checkbox.setValue((Boolean) workflowParameter.getComputedValue());
        checkbox.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(checkbox)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(checkbox::getValue)
                .valueAsStringSupplier(() -> valueOf(checkbox.getValue()))
                .build())
            .build();
    }

    @SuppressWarnings("unchecked")
    private static ComponentSupplierObject createStringSelect(WorkflowParameter workflowParameter) {
        final var singleValues = (List<String>) workflowParameter.getComputedValue();
        final var stringSelect = new Select<String>();
        stringSelect.setItems(singleValues);
        stringSelect.setValue(singleValues.getFirst());
        stringSelect.setWidthFull();
        stringSelect.setEmptySelectionAllowed(false);
        return ComponentSupplierObject.builder()
            .component(stringSelect)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(stringSelect::getValue)
                .valueAsStringSupplier(() -> getAllItemsWithUserSelectionFirst(stringSelect.getValue(), singleValues))
                .build())
            .build();
    }

    private static ComponentSupplierObject createTextField(WorkflowParameter workflowParameter) {
        final var textField = new TextField();
        textField.setValue((String) workflowParameter.getComputedValue());
        textField.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(textField)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(textField::getValue)
                .valueAsStringSupplier(textField::getValue)
                .build())
            .build();
    }

    private static <T> String getAllItemsWithUserSelectionFirst(T userSelection, Collection<T> allItems) {
        final var stringBuilder = new StringBuilder();
        stringBuilder.append(userSelection.toString());
        stringBuilder.append(",");
        for (T item : allItems) {
            final var itemAsString = item.toString();
            if (itemAsString.equals(userSelection)) {
                continue;
            }
            stringBuilder.append(itemAsString);
            stringBuilder.append(",");
        }
        return stringBuilder.deleteCharAt(stringBuilder.length() - 1).toString();
    }

    @Getter
    @Builder
    public static class ComponentSupplierObject {
        private final Component component;
        private final ComponentValueSupplier componentValueSupplier;
    }

    @Getter
    @Builder
    public static class ComponentValueSupplier {
        private final Supplier<Object> valueSupplier;
        private final Supplier<String> valueAsStringSupplier;
    }

}
