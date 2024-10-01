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
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.Validator;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowDevParamGrid extends Composite<VerticalLayout> {

    private final Grid<WorkflowParameter> grid = new Grid<>(WorkflowParameter.class, false);

    private final List<WorkflowParameter> workflowParameters = new ArrayList<>();

    public WorkflowDevParamGrid() {
        grid.getElement().getStyle().set("max-height", "250px");
        grid.getElement().getStyle().set("overflow", "auto");
    }

    public Map<String, Object> getParametersAsMap() {
        return workflowParameters.stream()
            .collect(Collectors.toMap(WorkflowParameter::getName, WorkflowParameter::getComputedValue, (it1, it2) -> it2));
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        final var editor = grid.getEditor();
        final var binder = new Binder<>(WorkflowParameter.class);
        editor.setBinder(binder);
        editor.setBuffered(true);

        final var nameField = new TextField();
        nameField.setWidthFull();
        binder.forField(nameField)
            .asRequired(getTranslation("workflow-dev-param.grid.name.required"))
            .bind(WorkflowParameter::getName, WorkflowParameter::setName);
        final var nameColumn = grid.addColumn(WorkflowParameter::getName).setHeader(getTranslation("workflow-dev-param.grid.name.column"));
        nameColumn.setEditorComponent(nameField);

        ComboBox<WorkflowParameterType> typeField = new ComboBox<>();
        typeField.setItems(WorkflowParameterType.values());
        typeField.setWidthFull();
        binder.forField(typeField)
            .bind(WorkflowParameter::getType, WorkflowParameter::setType);
        final var typeColumn = grid.addColumn(WorkflowParameter::getType).setHeader(getTranslation("workflow-dev-param.grid.type.column"));
        typeColumn.setEditorComponent(typeField);

        final var valueField = new TextField();
        valueField.setWidthFull();
        binder.forField(valueField)
            .withValidator((Validator<String>) (value, valueContext) -> WorkflowParameter.validateTypeAndValue(typeField.getValue(), value)
                        .map(ValidationResult::error)
                        .orElseGet(ValidationResult::ok))
            .bind(WorkflowParameter::getValue, WorkflowParameter::setValue);
        final var valueColumn = grid.addColumn(WorkflowParameter::getValue).setHeader(getTranslation("workflow-dev-param.grid.value.column"));
        valueColumn.setEditorComponent(valueField);

        final var saveButton = new Button(VaadinIcon.CHECK.create(), event -> editor.save());
        saveButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_SUCCESS, ButtonVariant.LUMO_SMALL);
        final var cancelButton = new Button(VaadinIcon.CLOSE.create(), event -> editor.cancel());
        cancelButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_ERROR, ButtonVariant.LUMO_SMALL);
        final var actions = new HorizontalLayout(saveButton, cancelButton);
        actions.setPadding(false);

        final var addButton = new Button(VaadinIcon.PLUS.create());
        addButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_PRIMARY, ButtonVariant.LUMO_SMALL);
        addButton.addClickListener(event -> {
            final var parameter = new WorkflowParameter();
            parameter.setName("name");
            parameter.setValue("value");
            parameter.setType(WorkflowParameterType.STR);
            workflowParameters.add(parameter);
            grid.getDataProvider().refreshAll();
        });
        final var actionColumn = grid.addComponentColumn(parameter -> createActionComponent(parameter, editor))
                .setHeader(addButton);
        actionColumn.setEditorComponent(actions);

        grid.setItems(workflowParameters);
        grid.addThemeVariants(GridVariant.LUMO_COMPACT);

        layout.add(grid);
        return layout;
    }

    private Component createActionComponent(WorkflowParameter workflowParameter, Editor<WorkflowParameter> editor) {
        final var horizontalLayout = new HorizontalLayout();
        final var editButton = new Button(VaadinIcon.EDIT.create());
        editButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_SMALL);
        editButton.addClickListener(event -> {
            if (editor.isOpen()) {
                editor.cancel();
            }
            grid.getEditor().editItem(workflowParameter);
        });
        final var deleteButton = new Button(VaadinIcon.TRASH.create());
        deleteButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_ERROR, ButtonVariant.LUMO_SMALL);
        deleteButton.addClickListener(event -> {
            workflowParameters.remove(workflowParameter);
            grid.getDataProvider().refreshAll();
        });
        horizontalLayout.add(editButton, deleteButton);
        return horizontalLayout;
    }

}
