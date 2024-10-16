package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
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
            .setHeader(getTranslation("workflow-runs.main-grid.id.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderTemplateId))
            .setHeader(getTranslation("workflow-runs.main-grid.template.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderStatus))
            .setHeader(getTranslation("workflow-runs.main-grid.status.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderDetails))
            .setHeader(getTranslation("workflow-runs.main-grid.details.column"))
            .setAutoWidth(true);
        paginatedGrid.setEmptyStateText(getTranslation("workflow-runs.main-grid.no-result"));
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
            final var span = new Span(getTranslation("workflow-runs.main-grid.template.manual"));
            span.getElement().getThemeList().add("badge contrast small");
            return span;
        });
    }

    private Component renderStatus(WorkflowRun workflowRun) {
        return ofNullable(workflowRun.getFailureMessage()).map(item -> {
            final var errorSpan = new Span(getTranslation("workflow-runs.main-grid.status.error", workflowRun.getHumanReadableDuration()));
            errorSpan.getElement().getThemeList().add("badge error small");
            return errorSpan;
        }).orElseGet(() -> {
            if (workflowRun.getDuration() >= 0) {
                final var successSpan = new Span(getTranslation("workflow-runs.main-grid.status.success", workflowRun.getHumanReadableDuration()));
                successSpan.getElement().getThemeList().add("badge success small");
                return successSpan;
            } else {
                final var pendingSpan = new Span(getTranslation("workflow-runs.main-grid.status.pending", workflowRun.getHumanReadableDuration()));
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

}
