package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.UserRole;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.handler.WorkflowDefinitionEventHandler;
import org.myworkflows.view.WorkflowDevelopmentView;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.component.html.TextFieldWithEnterShortcut;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowDefinitionGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowDefinition, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final WorkflowDefinitionEventHandler workflowDefinitionEventHandler;

    private final boolean isLoggedAsAdmin = UserRole.ADMIN.validate();

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowDefinition, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-definitions.grid.name.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(new TextFieldWithEnterShortcut(workflowDefinitionEventHandler::onCreate).small())
            .setAutoWidth(true);

        layout.add(paginatedGrid);
        return layout;
    }

    private Component renderName(WorkflowDefinition workflowDefinition) {
        if (!workflowDefinition.isEditable()) {
            final var routerLink = new RouterLink(workflowDefinition.getName(), WorkflowDevelopmentView.class, workflowDefinition.getId());
            routerLink.getElement().getThemeList().add("badge");
            return routerLink;
        }

        final var nameTextField = new TextField();
        nameTextField.setSuffixComponent(VaadinIcon.ENTER.create());
        nameTextField.setValue(workflowDefinition.getName());
        addShortcutListener(nameTextField, () -> {
            workflowDefinitionEventHandler.onUpdate(workflowDefinition, nameTextField.getValue());
            onUpdated(workflowDefinition);
        }, Key.ENTER);
        addShortcutListener(nameTextField, () -> onUpdated(workflowDefinition), Key.ESCAPE);

        return nameTextField;
    }

    private Component renderActions(WorkflowDefinition workflowDefinition) {
        final var layout = new HorizontalLayout();

        final var editButton = new Button(new Icon(VaadinIcon.EDIT));
        editButton.setTooltipText(getTranslation("workflow-definitions.grid.actions.button.edit.title"));
        editButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var deleteButton = new Button();
        deleteButton.setIcon(new Icon(VaadinIcon.TRASH));
        deleteButton.setTooltipText(getTranslation("workflow-definitions.grid.actions.button.delete.title"));
        deleteButton.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_ERROR);

        if (isLoggedAsAdmin) {
            editButton.addClickListener(event -> onEdit(workflowDefinition));
            deleteButton.addClickListener(event -> workflowDefinitionEventHandler.onDelete(workflowDefinition));
        } else {
            editButton.setEnabled(false);
            deleteButton.setEnabled(false);
        }

        layout.add(editButton, deleteButton);
        return layout;
    }

    private void onEdit(WorkflowDefinition workflowDefinition) {
        workflowDefinition.toggleOnEditing();
        paginatedGrid.getDataProvider().refreshItem(workflowDefinition);
    }

    private void onUpdated(WorkflowDefinition workflowDefinition) {
        workflowDefinition.setEditable(false);
        paginatedGrid.getDataProvider().refreshItem(workflowDefinition);
    }

}
