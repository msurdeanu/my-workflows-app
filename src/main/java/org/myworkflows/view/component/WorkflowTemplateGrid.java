package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import com.vaadin.flow.theme.lumo.LumoIcon;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.view.WorkflowDevelopmentView;
import org.vaadin.klaudeta.PaginatedGrid;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowTemplateGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowTemplate, ?> paginatedGrid = new PaginatedGrid<>();

    private final WorkflowTemplateEventHandler workflowTemplateEventHandler;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(final DataProvider<WorkflowTemplate, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        paginatedGrid.setAllRowsVisible(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActive))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-templates.main-grid.name.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(getTranslation("workflow-templates.main-grid.actions.column"))
            .setAutoWidth(true);
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(
            () -> new WorkflowTemplateDetails(workflowTemplateEventHandler),
            WorkflowTemplateDetails::setDetails)
        );
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderActive(final WorkflowTemplate workflowTemplate) {
        final var checkbox = new Checkbox(workflowTemplate.isEnabled());
        checkbox.addValueChangeListener(event -> workflowTemplateEventHandler.onActivationChanged(workflowTemplate.getId()));
        return checkbox;
    }

    private Component renderName(final WorkflowTemplate workflowTemplate) {
        final var layout = new HorizontalLayout();
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

    private Component getOnlyName(final WorkflowTemplate workflowTemplate) {
        final var routerLink = new RouterLink(workflowTemplate.getName(), WorkflowDevelopmentView.class, workflowTemplate.getId());
        routerLink.getElement().getThemeList().add("badge" + (workflowTemplate.isEnabled() ? "" : " contrast"));
        return routerLink;
    }

    private Component renderActions(final WorkflowTemplate workflowTemplate) {
        final var layout = new HorizontalLayout();

        final var scheduleNowButton = new Button(new Icon(VaadinIcon.START_COG));
        scheduleNowButton.addThemeVariants(ButtonVariant.LUMO_ICON);
        scheduleNowButton.setTooltipText(getTranslation("workflow-templates.main-grid.actions.button.schedule.title"));
        scheduleNowButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        final var deleteButton = new Button();
        deleteButton.setIcon(new Icon(VaadinIcon.TRASH));
        deleteButton.setTooltipText(getTranslation("workflow-templates.main-grid.actions.button.delete.title"));
        deleteButton.addThemeVariants(ButtonVariant.LUMO_SMALL);

        // TODO
        scheduleNowButton.addClickListener(event -> onScheduleNow(workflowTemplate));
        if (!workflowTemplate.isEnabled()) {
            deleteButton.addClickListener(event -> workflowTemplateEventHandler.onDelete(workflowTemplate.getId()));
        } else {
            deleteButton.setEnabled(false);
        }

        layout.add(scheduleNowButton, deleteButton);
        return layout;
    }

    private void onScheduleNow(final WorkflowTemplate workflowTemplate) {
        workflowTemplateEventHandler.onScheduleNow(workflowTemplate.getId());
        paginatedGrid.getDataProvider().refreshItem(workflowTemplate);
    }

}
