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
import org.myworkflows.domain.WorkflowRun;
import org.vaadin.klaudeta.PaginatedGrid;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowRun, ?> paginatedGrid = new PaginatedGrid<>();

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
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderWorkflowId))
            .setHeader(getTranslation("workflow-runs.main-grid.id.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderDuration))
            .setHeader(getTranslation("workflow-runs.main-grid.duration.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderDetails))
            .setHeader(getTranslation("workflow-runs.main-grid.details.column"))
            .setAutoWidth(true);
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderWorkflowId(WorkflowRun workflowRun) {
        return new Span(String.valueOf(workflowRun.getId()));
    }

    private Component renderDuration(WorkflowRun workflowRun) {
        return new Span(workflowRun.getHumanReadableDuration());
    }

    private Component renderDetails(WorkflowRun workflowRun) {
        final var button = new Button("TODO"); // TODO
        button.addThemeVariants(ButtonVariant.LUMO_SMALL);
        button.setTooltipText(workflowRun.getCreated().toString());
        button.addClickListener(event -> new WorkflowRunDetailsDialog(workflowRun).open());
        return button;
    }

}
