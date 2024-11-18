package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Span;
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
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.view.WorkflowDevelopmentView;
import org.myworkflows.view.component.html.SpanBadge;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.component.html.TextFieldWithEnterShortcut;

import java.util.List;

import static com.vaadin.flow.component.Shortcuts.addShortcutListener;
import static java.util.Optional.ofNullable;
import static org.myworkflows.util.ListUtil.substract;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowTemplateGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowTemplate, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final WorkflowTemplateEventHandler workflowTemplateEventHandler;

    private final List<WorkflowDefinition> allWorkflowDefinitions;

    private final List<WorkflowParameter> allWorkflowParameters;

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

        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActive))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderNameAndCron))
            .setHeader(getTranslation("workflow-templates.grid.name.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(new TextFieldWithEnterShortcut(workflowTemplateEventHandler::onCreate).small())
            .setAutoWidth(true);
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(this::createDraggableComponent));

        layout.add(paginatedGrid);
        return layout;
    }

    private Component renderActive(WorkflowTemplate workflowTemplate) {
        final var checkbox = new Checkbox(workflowTemplate.isEnabled());
        checkbox.addValueChangeListener(event -> workflowTemplateEventHandler.onActivationChanged(workflowTemplate));
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
                        .withText(getTranslation("workflow-templates.grid.cron-expression.tooltip", cron))
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
            workflowTemplateEventHandler.onNameAndCronUpdated(workflowTemplate, nameTextField.getValue(), cronTextField.getValue());
            onUpdated(workflowTemplate);
        }, Key.ENTER);
        addShortcutListener(layout, () -> onUpdated(workflowTemplate), Key.ESCAPE);

        return layout;
    }

    private Component getOnlyName(WorkflowTemplate workflowTemplate) {
        return new SpanBadge(workflowTemplate.getName(), workflowTemplate.isEnabled() ? StringUtils.EMPTY : "contrast");
    }

    private Component renderActions(WorkflowTemplate workflowTemplate) {
        final var layout = new HorizontalLayout();

        final var scheduleNowButton = new Button(VaadinIcon.START_COG.create());
        scheduleNowButton.setTooltipText(getTranslation("workflow-templates.grid.actions.button.schedule.title"));
        scheduleNowButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var editButton = new Button(VaadinIcon.EDIT.create());
        editButton.setTooltipText(getTranslation("workflow-templates.grid.actions.button.edit.title"));
        editButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var deleteButton = new Button(VaadinIcon.TRASH.create());
        deleteButton.setTooltipText(getTranslation("workflow-templates.grid.actions.button.delete.title"));
        deleteButton.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_ERROR);

        if (isLogged) {
            scheduleNowButton.addClickListener(event -> onScheduleNow(workflowTemplate));
        } else {
            scheduleNowButton.setEnabled(false);
        }
        if (isLoggedAsAdmin) {
            editButton.addClickListener(event -> onEdit(workflowTemplate));
            if (!workflowTemplate.isEnabled()) {
                deleteButton.addClickListener(event -> workflowTemplateEventHandler.onDelete(workflowTemplate));
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

    private Component createDraggableComponent(WorkflowTemplate workflowTemplate) {
        final var verticalLayout = new VerticalLayout();
        final var definitionDraggableGrid = new DraggableGrid<WorkflowTemplate, WorkflowDefinition>("definition",
            definition -> new Span(definition.getId().toString()),
            definition -> {
                final var routerLink = new RouterLink(definition.getName(), WorkflowDevelopmentView.class, definition.getId());
                routerLink.getElement().getThemeList().add("badge");
                return routerLink;
            },
            WorkflowDefinition.class);
        definitionDraggableGrid.setDetails(workflowTemplate, workflowTemplateEventHandler::onDefinitionUpdated,
            WorkflowTemplate::getWorkflowDefinitions, substract(allWorkflowDefinitions, workflowTemplate.getWorkflowDefinitions()));
        final var parameterDraggableGrid = new DraggableGrid<WorkflowTemplate, WorkflowParameter>("parameter",
            parameter -> new Span(parameter.getName()),
            parameter -> new Span(parameter.getType().getValue()),
            WorkflowParameter.class);
        parameterDraggableGrid.setDetails(workflowTemplate, workflowTemplateEventHandler::onParameterUpdated,
            WorkflowTemplate::getWorkflowParameters, substract(allWorkflowParameters, workflowTemplate.getWorkflowParameters()));
        verticalLayout.add(definitionDraggableGrid, parameterDraggableGrid);
        return verticalLayout;
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
        workflowTemplateEventHandler.onScheduleNow(workflowTemplate);
        paginatedGrid.getDataProvider().refreshItem(workflowTemplate);
    }

}
