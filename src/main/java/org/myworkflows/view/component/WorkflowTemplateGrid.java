package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.NativeLabel;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import com.vaadin.flow.theme.lumo.LumoIcon;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.UserRole;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.view.WorkflowDevelopmentView;
import org.vaadin.klaudeta.PaginatedGrid;

import java.util.List;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;
import static java.util.Optional.ofNullable;
import static org.myworkflows.util.StreamUtil.substract;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowTemplateGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowTemplate, ?> paginatedGrid = new PaginatedGrid<>();

    private final WorkflowTemplateEventHandler workflowTemplateEventHandler;

    private final List<WorkflowDefinition> allWorkflowDefinitions;

    private final boolean isLoggedAsAdmin = UserRole.ADMIN.validate();

    private final boolean isLogged = UserRole.LOGGED.validate();

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowTemplate, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        paginatedGrid.setAllRowsVisible(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActive))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderNameAndCron))
            .setHeader(getTranslation("workflow-templates.main-grid.name.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(getTranslation("workflow-templates.main-grid.actions.column"))
            .setAutoWidth(true);
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(
            () -> new DraggableGrid<WorkflowTemplate, WorkflowDefinition>("template",
                definition -> new NativeLabel(definition.getId().toString()),
                definition -> {
                    final var routerLink = new RouterLink(definition.getName(), WorkflowDevelopmentView.class, definition.getId());
                    routerLink.getElement().getThemeList().add("badge");
                    return routerLink;
                },
                WorkflowDefinition.class),
            (instance, workflowTemplate) -> instance.setDetails(workflowTemplate,
                (key, value) -> workflowTemplateEventHandler.onDefinitionUpdated(key.getId(), value),
                WorkflowTemplate::getWorkflowDefinitions,
                substract(allWorkflowDefinitions, workflowTemplate.getWorkflowDefinitions())))
        );
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderActive(WorkflowTemplate workflowTemplate) {
        final var checkbox = new Checkbox(workflowTemplate.isEnabled());
        checkbox.addValueChangeListener(event -> workflowTemplateEventHandler.onActivationChanged(workflowTemplate.getId()));
        return checkbox;
    }

    private Component renderNameAndCron(WorkflowTemplate workflowTemplate) {
        final var layout = new HorizontalLayout();
        if (!workflowTemplate.isEditable()) {
            return ofNullable(workflowTemplate.getCron())
                .<Component>map(cron -> {
                    layout.add(getOnlyName(workflowTemplate));
                    final var icon = LumoIcon.CLOCK.create();
                    Tooltip.forComponent(icon)
                        .withText(getTranslation("workflow-templates.main-grid.cron-expression.tooltip", cron))
                        .withPosition(Tooltip.TooltipPosition.TOP);
                    layout.add(icon);
                    layout.setSpacing(false);
                    return layout;
                })
                .orElseGet(() -> getOnlyName(workflowTemplate));
        }

        final var nameTextField = new TextField();
        nameTextField.setSuffixComponent(VaadinIcon.ENTER.create());
        nameTextField.setValue(workflowTemplate.getName());
        layout.add(nameTextField);
        final var cronTextField = new TextField();
        cronTextField.setSuffixComponent(VaadinIcon.ENTER.create());
        cronTextField.setValue(ofNullable(workflowTemplate.getCron()).orElse(StringUtils.EMPTY));
        layout.add(cronTextField);

        addShortcutListener(layout, () -> {
            workflowTemplateEventHandler.onNameAndCronUpdated(workflowTemplate.getId(), nameTextField.getValue(), cronTextField.getValue());
            onUpdated(workflowTemplate);
        }, Key.ENTER);
        addShortcutListener(layout, () -> onUpdated(workflowTemplate), Key.ESCAPE);

        return layout;
    }

    private Component getOnlyName(WorkflowTemplate workflowTemplate) {
        final var nativeLabel = new NativeLabel(workflowTemplate.getName());
        nativeLabel.getElement().getThemeList().add("badge" + (workflowTemplate.isEnabled() ? "" : " contrast"));
        return nativeLabel;
    }

    private Component renderActions(WorkflowTemplate workflowTemplate) {
        final var layout = new HorizontalLayout();

        final var scheduleNowButton = new Button(new Icon(VaadinIcon.START_COG));
        scheduleNowButton.addThemeVariants(ButtonVariant.LUMO_ICON);
        scheduleNowButton.setTooltipText(getTranslation("workflow-templates.main-grid.actions.button.schedule.title"));
        scheduleNowButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var editButton = new Button(new Icon(VaadinIcon.EDIT));
        editButton.setTooltipText(getTranslation("workflow-templates.main-grid.actions.button.edit.title"));
        editButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var deleteButton = new Button();
        deleteButton.setIcon(new Icon(VaadinIcon.TRASH));
        deleteButton.setTooltipText(getTranslation("workflow-templates.main-grid.actions.button.delete.title"));
        deleteButton.addThemeVariants(ButtonVariant.LUMO_SMALL);

        if (isLogged) {
            scheduleNowButton.addClickListener(event -> onScheduleNow(workflowTemplate));
        } else {
            scheduleNowButton.setEnabled(false);
        }
        if (isLoggedAsAdmin) {
            editButton.addClickListener(event -> onEdit(workflowTemplate));
            if (!workflowTemplate.isEnabled()) {
                deleteButton.addClickListener(event -> workflowTemplateEventHandler.onDelete(workflowTemplate.getId()));
            } else {
                deleteButton.setEnabled(false);
            }
        } else {
            editButton.setEnabled(false);
            deleteButton.setEnabled(false);
        }

        layout.add(scheduleNowButton, editButton, deleteButton);
        return layout;
    }

    private void onEdit(WorkflowTemplate workflowTemplate) {
        workflowTemplate.toggleOnEditing();
        paginatedGrid.getDataProvider().refreshItem(workflowTemplate);
    }

    private void onUpdated(WorkflowTemplate workflowTemplate) {
        workflowTemplate.setEditable(false);
        paginatedGrid.getDataProvider().refreshItem(workflowTemplate);
    }

    private void onScheduleNow(WorkflowTemplate workflowTemplate) {
        workflowTemplateEventHandler.onScheduleNow(workflowTemplate.getId());
        paginatedGrid.getDataProvider().refreshItem(workflowTemplate);
    }

}
