package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.grid.editor.Editor;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import org.myworkflows.domain.Parameter;
import org.myworkflows.domain.ParameterType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowParameterGrid extends Composite<VerticalLayout> {

    private final Grid<Parameter> grid = new Grid<>(Parameter.class, false);

    private final List<Parameter> workflowParameters = new ArrayList<>();

    public WorkflowParameterGrid() {
        Parameter parameter = new Parameter();
        parameter.setName("sleepTime");
        parameter.setValue("5000");
        parameter.setType(ParameterType.INT);
        workflowParameters.add(parameter);
    }

    public Map<String, Object> getParametersAsMap() {
        return workflowParameters.stream()
            .collect(Collectors.toMap(Parameter::getName, Parameter::getComputedValue, (it1, it2) -> it2));
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        Editor<Parameter> editor = grid.getEditor();

        Binder<Parameter> binder = new Binder<>(Parameter.class);
        editor.setBinder(binder);
        editor.setBuffered(true);

        TextField nameField = new TextField();
        nameField.setWidthFull();
        binder.forField(nameField)
            .asRequired("Parameter name is required")
            .bind(Parameter::getName, Parameter::setName);
        final var nameColumn = grid.addColumn(Parameter::getName).setHeader("Name");
        nameColumn.setEditorComponent(nameField);

        ComboBox<ParameterType> typeField = new ComboBox<>();
        typeField.setItems(ParameterType.values());
        typeField.setWidthFull();
        binder.forField(typeField)
            .bind(Parameter::getType, Parameter::setType);
        final var typeColumn = grid.addColumn(Parameter::getType).setHeader("Type");
        typeColumn.setEditorComponent(typeField);

        TextField valueField = new TextField();
        valueField.setWidthFull();
        binder.forField(valueField)
            .asRequired("Parameter value must not be empty")
            .bind(Parameter::getValue, Parameter::setValue);
        final var valueColumn = grid.addColumn(Parameter::getValue).setHeader("Value");
        valueColumn.setEditorComponent(valueField);

        final var saveButton = new Button(VaadinIcon.CHECK.create(), event -> editor.save());
        saveButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_SUCCESS, ButtonVariant.LUMO_SMALL);
        final var cancelButton = new Button(VaadinIcon.CLOSE.create(), event -> editor.cancel());
        cancelButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_ERROR, ButtonVariant.LUMO_SMALL);
        HorizontalLayout actions = new HorizontalLayout(saveButton, cancelButton);
        actions.setPadding(false);
        final var actionColumn = grid.addComponentColumn(parameter -> createActionComponent(parameter, editor));
        actionColumn.setEditorComponent(actions);

        grid.setItems(workflowParameters);
        grid.addThemeVariants(GridVariant.LUMO_COMPACT);

        layout.add(grid);
        return layout;
    }

    private Component createActionComponent(Parameter parameter, Editor<Parameter> editor) {
        final var horizontalLayout = new HorizontalLayout();
        final var editButton = new Button("Edit");
        editButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        editButton.addClickListener(event -> {
            if (editor.isOpen()) {
                editor.cancel();
            }
            grid.getEditor().editItem(parameter);
        });
        final var deleteButton = new Button("Delete");
        deleteButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        deleteButton.addClickListener(event -> {
            if (editor.isOpen()) {
                editor.cancel();
            }
            workflowParameters.remove(parameter);
            grid.getDataProvider().refreshAll();
        });
        horizontalLayout.add(editButton, deleteButton);
        return horizontalLayout;
    }

}
