package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionEventHandler;
import org.myworkflows.view.WorkflowDevelopmentView;
import org.vaadin.klaudeta.PaginatedGrid;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowDefinitionGrid extends Composite<VerticalLayout> {

    private final PaginatedGrid<WorkflowDefinition, ?> paginatedGrid = new PaginatedGrid<>();

    private final WorkflowDefinitionEventHandler workflowDefinitionEventHandler;

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
        paginatedGrid.setAllRowsVisible(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-definitions.main-grid.name.column"))
            .setAutoWidth(true);
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(
            () -> new WorkflowDefinitionDetails(workflowDefinitionEventHandler),
            WorkflowDefinitionDetails::setDetails)
        );
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        return layout;
    }

    private Component renderName(WorkflowDefinition workflowDefinition) {
        final var routerLink = new RouterLink(workflowDefinition.getName(), WorkflowDevelopmentView.class, workflowDefinition.getId());
        routerLink.getElement().getThemeList().add("badge");
        return routerLink;
    }

}
