package org.myworkflows.view.transformer;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBoxVariant;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.datepicker.DatePickerVariant;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.select.SelectVariant;
import com.vaadin.flow.component.textfield.EmailField;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextAreaVariant;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldVariant;
import com.vaadin.flow.component.timepicker.TimePicker;
import com.vaadin.flow.component.timepicker.TimePickerVariant;
import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinService;
import jakarta.servlet.http.Cookie;
import lombok.Builder;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowParameter;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static java.lang.String.valueOf;
import static java.util.Optional.ofNullable;
import static org.myworkflows.util.CookieUtil.createEncryptedCookie;
import static org.myworkflows.util.CookieUtil.findDecryptedCookieValue;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowParameterToComponentSupplierObjectTransformer
    implements Transformer<WorkflowParameter, WorkflowParameterToComponentSupplierObjectTransformer.ComponentSupplierObject> {

    private static final String COMMA = ",";

    @Override
    public ComponentSupplierObject transform(WorkflowParameter workflowParameter) {
        final var workflowParameterType = workflowParameter.getType();
        return switch (workflowParameterType) {
            case DATE -> createDateField(workflowParameter);
            case TIME -> createTimeField(workflowParameter);
            case PASS -> createPasswordField(workflowParameter);
            case INT -> createIntegerField(workflowParameter);
            case DOUBLE -> createNumberField(workflowParameter);
            case BOOL -> createCheckboxField(workflowParameter);
            case S_STR -> createStringSelect(workflowParameter);
            case MS_STR -> createMultiStringSelect(workflowParameter);
            case EMAIL -> createEmailField(workflowParameter);
            case M_STR -> createTextArea(workflowParameter);
            default -> createTextField(workflowParameter);
        };
    }

    private static ComponentSupplierObject createDateField(WorkflowParameter workflowParameter) {
        final var datePicker = new DatePicker();
        datePicker.addThemeVariants(DatePickerVariant.LUMO_SMALL);
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

    private static ComponentSupplierObject createTimeField(WorkflowParameter workflowParameter) {
        final var timePicker = new TimePicker();
        timePicker.addThemeVariants(TimePickerVariant.LUMO_SMALL);
        timePicker.setValue((LocalTime) workflowParameter.getComputedValue());
        timePicker.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(timePicker)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(timePicker::getValue)
                .valueAsStringSupplier(() -> timePicker.getValue().toString())
                .build())
            .build();
    }

    private static ComponentSupplierObject createPasswordField(WorkflowParameter workflowParameter) {
        final var allCookies = getAllCookies();
        final var passwordField = new PasswordField();
        passwordField.addThemeVariants(TextFieldVariant.LUMO_SMALL);
        passwordField.setValue(findDecryptedCookieValue(allCookies, workflowParameter.getName())
            .orElseGet(() -> (String) workflowParameter.getComputedValue()));
        passwordField.setWidthFull();
        final var cookieIcon = VaadinIcon.CLOUD_UPLOAD.create();
        cookieIcon.addClickListener(event -> createEncryptedCookie(allCookies, workflowParameter.getName(), passwordField.getValue())
            .ifPresent(cookie -> VaadinService.getCurrentResponse().addCookie(cookie)));
        passwordField.setPrefixComponent(cookieIcon);
        return ComponentSupplierObject.builder()
            .component(passwordField)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(passwordField::getValue)
                .valueAsStringSupplier(() -> StringUtils.EMPTY) // avoid to expose passwords when Share button is used
                .build())
            .build();
    }

    private static ComponentSupplierObject createIntegerField(WorkflowParameter workflowParameter) {
        final var integerField = new IntegerField();
        integerField.addThemeVariants(TextFieldVariant.LUMO_SMALL);
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
        numberField.addThemeVariants(TextFieldVariant.LUMO_SMALL);
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
        final var options = ((List<String>) workflowParameter.getComputedValue()).stream().map(Option::of).toList();
        final var stringSelect = new Select<Option>();
        stringSelect.addThemeVariants(SelectVariant.LUMO_SMALL);
        stringSelect.setItems(options);
        stringSelect.setItemLabelGenerator(Option::label);
        stringSelect.setValue(options.getFirst());
        stringSelect.setWidthFull();
        stringSelect.setEmptySelectionAllowed(false);
        return ComponentSupplierObject.builder()
            .component(stringSelect)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(() -> stringSelect.getValue().value())
                .valueAsStringSupplier(() -> getAllItemsWithUserSelectionFirst(stringSelect.getValue(), options))
                .build())
            .build();
    }

    @SuppressWarnings("unchecked")
    private static ComponentSupplierObject createMultiStringSelect(WorkflowParameter workflowParameter) {
        final var options = ((List<String>) workflowParameter.getComputedValue()).stream().map(Option::of).toList();
        final var multiStringSelect = new MultiSelectComboBox<Option>();
        multiStringSelect.addThemeVariants(MultiSelectComboBoxVariant.LUMO_SMALL);
        multiStringSelect.setItems(options);
        multiStringSelect.setItemLabelGenerator(Option::label);
        multiStringSelect.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(multiStringSelect)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(() -> multiStringSelect.getValue().stream().map(Option::value).collect(Collectors.toSet()))
                .valueAsStringSupplier(() -> getAllItemsWithUserSelectionFirst(null, options))
                .build())
            .build();
    }

    private static ComponentSupplierObject createEmailField(WorkflowParameter workflowParameter) {
        final var emailField = new EmailField();
        emailField.addThemeVariants(TextFieldVariant.LUMO_SMALL);
        emailField.setValue((String) workflowParameter.getComputedValue());
        emailField.setWidthFull();
        return ComponentSupplierObject.builder()
            .component(emailField)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(emailField::getValue)
                .valueAsStringSupplier(emailField::getValue)
                .build())
            .build();
    }

    private static ComponentSupplierObject createTextArea(WorkflowParameter workflowParameter) {
        final var textArea = new TextArea();
        textArea.addThemeVariants(TextAreaVariant.LUMO_SMALL);
        textArea.setValue((String) workflowParameter.getComputedValue());
        textArea.setWidthFull();
        textArea.setClearButtonVisible(true);
        return ComponentSupplierObject.builder()
            .component(textArea)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(textArea::getValue)
                .valueAsStringSupplier(textArea::getValue)
                .build())
            .build();
    }

    private static ComponentSupplierObject createTextField(WorkflowParameter workflowParameter) {
        final var textField = new TextField();
        textField.addThemeVariants(TextFieldVariant.LUMO_SMALL);
        textField.setValue((String) workflowParameter.getComputedValue());
        textField.setWidthFull();
        textField.setClearButtonVisible(true);
        return ComponentSupplierObject.builder()
            .component(textField)
            .componentValueSupplier(ComponentValueSupplier.builder()
                .valueSupplier(textField::getValue)
                .valueAsStringSupplier(textField::getValue)
                .build())
            .build();
    }

    private static String getAllItemsWithUserSelectionFirst(Option userSelection, Collection<Option> allItems) {
        final var stringBuilder = new StringBuilder();
        ofNullable(userSelection).ifPresent(option -> stringBuilder.append(option).append(COMMA));
        allItems.stream().filter(option -> !option.equals(userSelection)).map(Option::toString).forEach(option -> {
            stringBuilder.append(option);
            stringBuilder.append(COMMA);
        });
        return stringBuilder.deleteCharAt(stringBuilder.length() - 1).toString();
    }

    private static Cookie[] getAllCookies() {
        return ofNullable(VaadinService.getCurrentRequest())
            .map(VaadinRequest::getCookies)
            .orElseGet(() -> new Cookie[]{});
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

    private record Option(String label, String value) {

        private static final String SEPARATOR = "|";

        public String toString() {
            return label.equals(value) ? value : label + SEPARATOR + value;
        }

        public static Option of(String value) {
            final var labelIndex = value.indexOf(SEPARATOR);
            if (labelIndex > -1) {
                return new Option(value.substring(0, labelIndex), value.substring(labelIndex + 1));
            } else {
                return new Option(value, value);
            }
        }

    }

}
