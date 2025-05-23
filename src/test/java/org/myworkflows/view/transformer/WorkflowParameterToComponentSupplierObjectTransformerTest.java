package org.myworkflows.view.transformer;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.EmailField;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.timepicker.TimePicker;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;

import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowParameterToComponentSupplierObjectTransformerTest {

    @Test
    public void whenWorkflowParameterIsAStrThenATextFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.STR, "value"),
            field -> assertEquals("value", field.getValue()),
            componentValue -> assertEquals("value", componentValue), TextField.class);
    }

    @Test
    public void whenWorkflowParameterIsAnEmailThenAnEmailFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.EMAIL, "test@myworkflows.org"),
            field -> assertEquals("test@myworkflows.org", field.getValue()),
            componentValue -> assertEquals("test@myworkflows.org", componentValue), EmailField.class);
    }

    @Test
    public void whenWorkflowParameterIsAPassThenATextFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.PASS, "value"),
            field -> assertEquals("value", field.getValue()),
            componentValue -> assertEquals("", componentValue), PasswordField.class);
    }

    @Test
    public void whenWorkflowParameterIsADateThenADatePickerIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.DATE, "2024-01-20"),
            field -> assertEquals("2024-01-20", field.getValue().toString()),
            componentValue -> assertEquals("2024-01-20", componentValue), DatePicker.class);
    }

    @Test
    public void whenWorkflowParameterIsATimeThenATimePickerIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.TIME, "07:00"),
            field -> assertEquals("07:00", field.getValue().toString()),
            componentValue -> assertEquals("07:00", componentValue), TimePicker.class);
    }

    @Test
    public void whenWorkflowParameterIsAnIntThenAnIntegerFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.INT, "100"),
            field -> assertEquals(100, field.getValue()),
            componentValue -> assertEquals("100", componentValue), IntegerField.class);
    }

    @Test
    public void whenWorkflowParameterIsABoolThenACheckboxIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.BOOL, "false"),
            field -> assertEquals(Boolean.FALSE, field.getValue()),
            componentValue -> assertEquals("false", componentValue), Checkbox.class);
    }

    @Test
    public void whenWorkflowParameterIsADoubleThenANumberFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.DOUBLE, "10.5"),
            field -> assertEquals("10.5", field.getValue().toString()),
            componentValue -> assertEquals("10.5", componentValue), NumberField.class);
    }

    @Test
    public void whenWorkflowParameterIsASingleStringThenASelectFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.S_STR, "1,2"),
            field -> assertEquals("1", field.getValue().toString()),
            componentValue -> assertEquals("1,2", componentValue), Select.class);
    }

    @Test
    public void whenWorkflowParameterIsASingleStringWithLabelThenASelectFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.S_STR, "1|2,3|4"),
            field -> assertEquals("1|2", field.getValue().toString()),
            componentValue -> assertEquals("1|2,3|4", componentValue), Select.class);
    }

    @Test
    public void whenWorkflowParameterIsAMultiSingleStringThenASelectFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.MS_STR, "1,2"),
            field -> assertEquals("[]", field.getValue().toString()),
            componentValue -> assertEquals("1,2", componentValue), MultiSelectComboBox.class);
    }

    @Test
    public void whenWorkflowParameterIsAMultiSingleStringWithLabelThenASelectFieldIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.MS_STR, "1|2,3|4"),
            field -> assertEquals("[]", field.getValue().toString()),
            componentValue -> assertEquals("1|2,3|4", componentValue), MultiSelectComboBox.class);
    }

    @Test
    public void whenWorkflowParameterIsAMultiLineStringThenATextAreaIsCreated() {
        testScenario(WorkflowParameter.of("test", WorkflowParameterType.M_STR, "text\nlines"),
            field -> assertEquals("text\nlines", field.getValue()),
            componentValue -> assertEquals("text\nlines", componentValue), TextArea.class);
    }

    private <T> void testScenario(WorkflowParameter workflowParameter, Consumer<T> componentConsumer, Consumer<String> componentValueConsumer, Class<T> clazz) {
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(workflowParameter);
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(clazz, component);
        componentConsumer.accept(clazz.cast(component));
        componentValueConsumer.accept(componentValueSupplier.getValueAsStringSupplier().get());
    }

}
