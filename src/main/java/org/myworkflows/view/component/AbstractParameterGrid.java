package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.editor.Editor;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import com.vaadin.flow.data.binder.Validator;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.Setter;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.view.component.html.SpanBadge;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.component.html.TextFieldWithEnterShortcut;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class AbstractParameterGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowParameter, ?> paginatedGrid = new StandardPaginatedGrid<>();
    private final String id;

    @Setter
    private Function<WorkflowParameter, Component> renderValueFunction = workflowParameter -> new SpanBadge(workflowParameter.getValue());
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

        paginatedGrid.addColumn(WorkflowParameter::getName).setHeader(getTranslation(id + ".grid.name.column"));

        final var typeField = new ComboBox<WorkflowParameterType>();
        typeField.setItems(WorkflowParameterType.values());
        typeField.setWidthFull();
        typeField.setAllowCustomValue(false);
        typeField.setItemLabelGenerator(workflowParameterType -> getTranslation("workflow-parameter.type." + workflowParameterType.getValue()));
        binder.forField(typeField)
            .bind(WorkflowParameter::getType, WorkflowParameter::setType);
        paginatedGrid.addColumn(workflowParameter -> getTranslation("workflow-parameter.type." + workflowParameter.getType().getValue()))
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

        actionColumn = paginatedGrid.addComponentColumn(parameter -> createActionComponent(parameter, editor))
            .setHeader(new TextFieldWithEnterShortcut(createConsumer).allowedCharPatternAndPlaceholder("[a-zA-Z0-9_.]").small())
            .setEditorComponent(actions);

        layout.add(paginatedGrid);
        return layout;
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
