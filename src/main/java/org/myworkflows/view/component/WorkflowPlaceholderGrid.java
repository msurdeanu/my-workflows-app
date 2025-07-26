package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.grid.editor.Editor;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.handler.WorkflowPlaceholderEventHandler;
import org.myworkflows.util.PlaceholderUtil;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.component.html.TextFieldWithEnterShortcut;

import static java.lang.String.valueOf;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowPlaceholderGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowPlaceholder, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final WorkflowPlaceholderEventHandler workflowPlaceholderEventHandler;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowPlaceholder, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        final var editor = paginatedGrid.getEditor();
        final var binder = new Binder<>(WorkflowPlaceholder.class);
        editor.setBinder(binder);
        editor.setBuffered(true);

        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-placeholders.grid.name.column"));

        final var valueField = new TextField();
        valueField.setWidthFull();
        binder.forField(valueField)
            .bind(WorkflowPlaceholder::getValue, WorkflowPlaceholder::setValue);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValue))
            .setHeader(getTranslation("workflow-placeholders.grid.value.column"))
            .setEditorComponent(valueField);

        final var saveButton = new Button(VaadinIcon.CHECK.create(), event -> {
            final var currentItem = editor.getItem();
            if (editor.save()) {
                workflowPlaceholderEventHandler.onUpdate(currentItem);
            }
        });
        saveButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_SUCCESS, ButtonVariant.LUMO_SMALL);
        final var cancelButton = new Button(VaadinIcon.CLOSE.create(), event -> editor.cancel());
        cancelButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_ERROR, ButtonVariant.LUMO_SMALL);
        final var actions = new HorizontalLayout(saveButton, cancelButton);
        actions.setPadding(false);

        paginatedGrid.addComponentColumn(placeholder -> createActionComponent(placeholder, editor))
            .setHeader(new TextFieldWithEnterShortcut(item -> paginatedGrid.getEditor().editItem(workflowPlaceholderEventHandler.onCreate(item)))
                .allowedCharPatternAndPlaceholder(PlaceholderUtil.ALLOWED_CHARS_FOR_PLACEHOLDER_NAME).small())
            .setEditorComponent(actions);

        layout.add(paginatedGrid);
        return layout;
    }

    private Component renderName(WorkflowPlaceholder workflowPlaceholder) {
        return new Span(valueOf(workflowPlaceholder.getName()));
    }

    private Component renderValue(WorkflowPlaceholder workflowPlaceholder) {
        return new Span(valueOf(workflowPlaceholder.getValue()));
    }

    private Component createActionComponent(WorkflowPlaceholder workflowPlaceholder, Editor<WorkflowPlaceholder> editor) {
        final var layout = new HorizontalLayout();
        final var editButton = new Button(VaadinIcon.EDIT.create());
        editButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_SMALL);
        editButton.addClickListener(event -> {
            if (editor.isOpen()) {
                editor.cancel();
            }
            paginatedGrid.getEditor().editItem(workflowPlaceholder);
        });
        final var deleteButton = new Button(VaadinIcon.TRASH.create());
        deleteButton.addThemeVariants(ButtonVariant.LUMO_ICON, ButtonVariant.LUMO_ERROR, ButtonVariant.LUMO_SMALL);
        deleteButton.addClickListener(event -> new DeleteConfirmDialog(workflowPlaceholder.getName(),
            item -> workflowPlaceholderEventHandler.onDelete(workflowPlaceholder)).open());
        layout.add(editButton, deleteButton);
        return layout;
    }

}
