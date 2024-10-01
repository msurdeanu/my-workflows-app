package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.service.WorkflowParameterService;
import org.vaadin.klaudeta.PaginatedGrid;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowParameterGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowParameter, ?> paginatedGrid = new PaginatedGrid<>();

    private final WorkflowParameterService workflowParameterService;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowParameter, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        paginatedGrid.setAllRowsVisible(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
                .setHeader(getTranslation("workflow-params.main-grid.name.column"))
                .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderType))
                .setHeader(getTranslation("workflow-params.main-grid.type.column"))
                .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValue))
                .setHeader(getTranslation("workflow-params.main-grid.value.column"))
                .setAutoWidth(true);
        paginatedGrid.setEmptyStateText(getTranslation("workflow-params.main-grid.no-result"));
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderName(WorkflowParameter workflowParameter) {
        return new Span(workflowParameter.getName());
    }

    private Component renderType(WorkflowParameter workflowParameter) {
        return new Span(workflowParameter.getType().getValue());
    }

    private Component renderValue(WorkflowParameter workflowParameter) {
        return new Span(workflowParameter.getValue());
    }

}
