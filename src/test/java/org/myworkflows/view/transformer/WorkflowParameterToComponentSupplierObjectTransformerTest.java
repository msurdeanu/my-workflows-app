package org.myworkflows.view.transformer;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.NumberField;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextField;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;

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
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(WorkflowParameter.of("test", WorkflowParameterType.STR, "value"));
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(TextField.class, component);
        final var field = (TextField) component;
        assertEquals("value", field.getValue());
        assertEquals("value", componentValueSupplier.getValueAsStringSupplier().get());
    }

    @Test
    public void whenWorkflowParameterIsAPassThenATextFieldIsCreated() {
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(WorkflowParameter.of("test", WorkflowParameterType.PASS, "value"));
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(PasswordField.class, component);
        final var field = (PasswordField) component;
        assertEquals("value", field.getValue());
        assertEquals("value", componentValueSupplier.getValueAsStringSupplier().get());
    }

    @Test
    public void whenWorkflowParameterIsADateThenADatePickerIsCreated() {
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(WorkflowParameter.of("test", WorkflowParameterType.DATE, "2024-01-20"));
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(DatePicker.class, component);
        final var field = (DatePicker) component;
        assertEquals("2024-01-20", field.getValue().toString());
        assertEquals("2024-01-20", componentValueSupplier.getValueAsStringSupplier().get());
    }

    @Test
    public void whenWorkflowParameterIsAnIntThenAnIntegerFieldIsCreated() {
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(WorkflowParameter.of("test", WorkflowParameterType.INT, "100"));
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(IntegerField.class, component);
        final var field = (IntegerField) component;
        assertEquals(100, field.getValue());
        assertEquals("100", componentValueSupplier.getValueAsStringSupplier().get());
    }

    @Test
    public void whenWorkflowParameterIsABoolThenACheckboxIsCreated() {
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(WorkflowParameter.of("test", WorkflowParameterType.BOOL, "false"));
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(Checkbox.class, component);
        final var field = (Checkbox) component;
        assertEquals(Boolean.FALSE, field.getValue());
        assertEquals("false", componentValueSupplier.getValueAsStringSupplier().get());
    }

    @Test
    public void whenWorkflowParameterIsADoubleThenANumberFieldIsCreated() {
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(WorkflowParameter.of("test", WorkflowParameterType.DOUBLE, "10.5"));
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(NumberField.class, component);
        final var field = (NumberField) component;
        assertEquals("10.5", field.getValue().toString());
        assertEquals("10.5", componentValueSupplier.getValueAsStringSupplier().get());
    }

    @Test
    public void whenWorkflowParameterIsASingleStringThenASelectFieldIsCreated() {
        // given
        final var transformer = new WorkflowParameterToComponentSupplierObjectTransformer();

        // when & then
        final var componentSupplierObject = transformer.transform(WorkflowParameter.of("test", WorkflowParameterType.S_STR, "1,2"));
        assertNotNull(componentSupplierObject);
        final var component = componentSupplierObject.getComponent();
        final var componentValueSupplier = componentSupplierObject.getComponentValueSupplier();
        assertNotNull(component);
        assertNotNull(componentValueSupplier);
        assertInstanceOf(Select.class, component);
        final var field = (Select<String>) component;
        assertEquals("1", field.getValue());
        assertEquals("1,2", componentValueSupplier.getValueAsStringSupplier().get());
    }

}
