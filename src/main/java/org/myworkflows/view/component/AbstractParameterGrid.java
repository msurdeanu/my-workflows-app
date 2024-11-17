package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.grid.editor.Editor;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldVariant;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.Validator;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.data.value.ValueChangeMode;
import lombok.Setter;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.vaadin.klaudeta.PaginatedGrid;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class AbstractParameterGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowParameter, ?> paginatedGrid = new PaginatedGrid<>();
    private final String id;

    @Setter
    private Function<WorkflowParameter, Component> renderValueFunction = workflowParameter -> new Span(workflowParameter.getValue());
    @Setter
    private Consumer<String> createConsumer = value -> {
    };
    @Setter
    private Consumer<WorkflowParameter> updateConsumer = workflowParameter -> {
    };
    @Setter
    private Consumer<WorkflowParameter> deleteConsumer = workflowParameter -> {
    };

    private Grid.Column<WorkflowParameter> actionColumn;

    public AbstractParameterGrid(String id) {
        this.id = id;

        paginatedGrid.addClassName(id);
    }

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowParameter, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    public void setItems(List<WorkflowParameter> items) {
        paginatedGrid.setItems(items);
    }

    public void setReadOnly(boolean readOnly) {
        actionColumn.setVisible(!readOnly);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        final var editor = paginatedGrid.getEditor();
        final var binder = new Binder<>(WorkflowParameter.class);
        editor.setBinder(binder);
        editor.setBuffered(true);

        paginatedGrid.setAllRowsVisible(true);
        paginatedGrid.addColumn(WorkflowParameter::getName).setHeader(getTranslation(id + ".grid.name.column"));

        ComboBox<WorkflowParameterType> typeField = new ComboBox<>();
        typeField.setItems(WorkflowParameterType.values());
        typeField.setWidthFull();
        typeField.setAllowCustomValue(false);
        typeField.setRenderer(new ComponentRenderer<>(this::renderType));
        binder.forField(typeField)
            .bind(WorkflowParameter::getType, WorkflowParameter::setType);
        paginatedGrid.addColumn(WorkflowParameter::getType)
            .setHeader(getTranslation(id + ".grid.type.column"))
            .setEditorComponent(typeField);

        final var valueField = new TextField();
        valueField.setWidthFull();
        binder.forField(valueField)
            .withValidator((Validator<String>) (value, valueContext) -> WorkflowParameter.validateTypeAndValue(typeField.getValue(), value)
                .map(ValidationResult::error)
                .orElseGet(ValidationResult::ok))
            .bind(WorkflowParameter::getValue, WorkflowParameter::setValue);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValue))
            .setHeader(getTranslation(id + ".grid.value.column"))
            .setEditorComponent(valueField);

        final var saveButton = new Button(VaadinIcon.CHECK.create(), event -> {
            final var currentItem = editor.getItem();
            if (editor.save()) {
                updateConsumer.accept(currentItem);
            }
        });
        saveButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_SUCCESS, ButtonVariant.LUMO_SMALL);
        final var cancelButton = new Button(VaadinIcon.CLOSE.create(), event -> editor.cancel());
        cancelButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_ERROR, ButtonVariant.LUMO_SMALL);
        final var actions = new HorizontalLayout(saveButton, cancelButton);
        actions.setPadding(false);

        final var nameAndAddLayout = new HorizontalLayout();
        nameAndAddLayout.setWidthFull();
        nameAndAddLayout.setSpacing(true);
        final var nameField = new TextField();
        nameField.addThemeVariants(TextFieldVariant.LUMO_SMALL);
        nameField.setWidth("70%");
        nameField.setPlaceholder("[a-zA-Z0-9_.]");
        nameField.setAllowedCharPattern("[a-zA-Z0-9_.]");
        nameField.setValue("name");
        nameField.setClearButtonVisible(true);
        nameField.setValueChangeMode(ValueChangeMode.LAZY);
        nameField.setValueChangeTimeout(50);
        final var addButton = new Button(VaadinIcon.PLUS.create());
        addButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_PRIMARY, ButtonVariant.LUMO_SMALL);
        addButton.addClickListener(event -> createConsumer.accept(nameField.getValue()));
        nameAndAddLayout.add(nameField, addButton);
        nameAndAddLayout.setFlexGrow(1.0f, addButton);

        actionColumn = paginatedGrid.addComponentColumn(parameter -> createActionComponent(parameter, editor))
            .setHeader(nameAndAddLayout)
            .setEditorComponent(actions);

        paginatedGrid.setEmptyStateText(getTranslation(id + ".grid.no-result"));
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);

        layout.add(paginatedGrid);
        return layout;
    }

    private Component renderType(WorkflowParameterType workflowParameterType) {
        return new Span(getTranslation("workflow-parameter.type." + workflowParameterType.getValue()));
    }

    private Component renderValue(WorkflowParameter workflowParameter) {
        return renderValueFunction.apply(workflowParameter);
    }

    private Component createActionComponent(WorkflowParameter workflowParameter, Editor<WorkflowParameter> editor) {
        final var layout = new HorizontalLayout();
        final var editButton = new Button(VaadinIcon.EDIT.create());
        editButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_SMALL);
        editButton.addClickListener(event -> {
            if (editor.isOpen()) {
                editor.cancel();
            }
            paginatedGrid.getEditor().editItem(workflowParameter);
        });
        final var deleteButton = new Button(VaadinIcon.TRASH.create());
        deleteButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_ERROR, ButtonVariant.LUMO_SMALL);
        deleteButton.addClickListener(event -> deleteConsumer.accept(workflowParameter));
        layout.add(editButton, deleteButton);
        return layout;
    }

}
