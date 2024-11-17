package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.popover.Popover;
import com.vaadin.flow.component.popover.PopoverPosition;
import com.vaadin.flow.component.popover.PopoverVariant;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import com.vaadin.flow.theme.lumo.LumoUtility;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.view.WorkflowTemplateView;
import org.vaadin.klaudeta.PaginatedGrid;

import static java.lang.String.valueOf;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowRunGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowRun, ?> paginatedGrid = new PaginatedGrid<>();

    private final WorkflowRunService workflowRunService;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowRun, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        paginatedGrid.setAllRowsVisible(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderId))
            .setHeader(getTranslation("workflow-runs.grid.id.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderTemplateId))
            .setHeader(getTranslation("workflow-runs.grid.template.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderStatus))
            .setHeader(getTranslation("workflow-runs.grid.status.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderDetails))
            .setHeader(getTranslation("workflow-runs.grid.details.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(getTranslation("workflow-runs.grid.actions.column"))
            .setAutoWidth(true);
        paginatedGrid.setEmptyStateText(getTranslation("workflow-runs.grid.no-result"));
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderId(WorkflowRun workflowRun) {
        return new Span(valueOf(workflowRun.getId()));
    }

    private Component renderTemplateId(WorkflowRun workflowRun) {
        return workflowRunService.findWorkflowTemplate(workflowRun).map(template -> {
            final var routerLink = new RouterLink(template.getName(), WorkflowTemplateView.class, template.getId());
            routerLink.getElement().getThemeList().add("badge small");
            return (Component) routerLink;
        }).orElseGet(() -> {
            final var span = new Span(getTranslation("workflow-runs.grid.template.manual"));
            span.getElement().getThemeList().add("badge contrast small");
            return span;
        });
    }

    private Component renderStatus(WorkflowRun workflowRun) {
        return ofNullable(workflowRun.getFailureMessage()).map(item -> {
            final var errorSpan = new Span(getTranslation("workflow-runs.grid.status.error", workflowRun.getHumanReadableDuration()));
            errorSpan.getElement().getThemeList().add("badge error small");
            final var popover = new Popover();
            popover.setTarget(errorSpan);
            popover.setWidth("300px");
            popover.addThemeVariants(PopoverVariant.ARROW);
            popover.setPosition(PopoverPosition.BOTTOM);
            popover.add(createExceptionBlock(workflowRun));
            return errorSpan;
        }).orElseGet(() -> {
            if (workflowRun.getDuration() >= 0) {
                final var successSpan = new Span(getTranslation("workflow-runs.grid.status.success", workflowRun.getHumanReadableDuration()));
                successSpan.getElement().getThemeList().add("badge success small");
                return successSpan;
            } else {
                final var pendingSpan = new Span(getTranslation("workflow-runs.grid.status.pending", workflowRun.getHumanReadableDuration()));
                pendingSpan.getElement().getThemeList().add("badge contrast small");
                return pendingSpan;
            }
        });
    }

    private Component renderDetails(WorkflowRun workflowRun) {
        final var button = new Button(getTranslation("pretty.time.format", workflowRun.getCreated()));
        button.addThemeVariants(ButtonVariant.LUMO_SMALL);
        button.setTooltipText(workflowRun.getCreated().toString());
        button.addClickListener(event -> new WorkflowRunDetailsDialog(workflowRun).open());
        return button;
    }

    private Component renderActions(WorkflowRun workflowRun) {
        final var layout = new HorizontalLayout();
        if (workflowRun.isRunning()) {
            final var cancelRunButton = new Button(VaadinIcon.CLOSE.create());
            cancelRunButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
            cancelRunButton.addClickListener(event -> cancelRun(workflowRun));
            layout.add(cancelRunButton);
        } else {
            final var deleteRunButton = new Button(VaadinIcon.TRASH.create());
            deleteRunButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
            deleteRunButton.addClickListener(event -> deleteRun(workflowRun));
            layout.add(deleteRunButton);
        }
        return layout;
    }

    private void cancelRun(WorkflowRun workflowRun) {
        if (workflowRun.cancelAndInterruptIfRunning()) {
            Notification.show(getTranslation("workflow-runs.grid.run.cancelled"));
            paginatedGrid.getDataProvider().refreshItem(workflowRun);
        }
    }

    private void deleteRun(WorkflowRun workflowRun) {
        workflowRunService.delete(workflowRun);
        refreshPage();
    }

    private Component createExceptionBlock(WorkflowRun workflowRun) {
        final var span = new Span(workflowRun.getFailureMessage());
        span.addClassNames(LumoUtility.TextColor.ERROR);
        return span;
    }

}
