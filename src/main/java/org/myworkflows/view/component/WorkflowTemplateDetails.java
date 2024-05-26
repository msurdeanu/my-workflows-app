package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.dnd.GridDragEndEvent;
import com.vaadin.flow.component.grid.dnd.GridDragStartEvent;
import com.vaadin.flow.component.grid.dnd.GridDropLocation;
import com.vaadin.flow.component.grid.dnd.GridDropMode;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.view.WorkflowDevelopmentView;

import java.util.List;

import static com.vaadin.flow.component.grid.dnd.GridDropMode.BETWEEN;
import static com.vaadin.flow.component.grid.dnd.GridDropMode.ON_GRID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowTemplateDetails extends Composite<VerticalLayout> {

    private Div container = new Div();

    private final WorkflowTemplateEventHandler workflowTemplateEventHandler;

    private WorkflowDefinition draggedItem;

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.add(container);
        return layout;
    }

    public void setDetails(WorkflowTemplate workflowTemplate, List<WorkflowDefinition> availableWorkflowDefinitions) {
        final var templateWorkflowDefinitions = workflowTemplate.getWorkflowDefinitions();

        final var currentWorkflowDefinitionsGrid = setupGrid(getTranslation("workflow-templates.main-grid.details.current"), BETWEEN);
        final var currentDataView = currentWorkflowDefinitionsGrid.setItems(templateWorkflowDefinitions);
        final var availableWorkflowDefinitionsGrid = setupGrid(getTranslation("workflow-templates.main-grid.details.available"), ON_GRID);
        final var availableDataView = availableWorkflowDefinitionsGrid.setItems(availableWorkflowDefinitions);

        currentWorkflowDefinitionsGrid.addDropListener(event -> {
            availableDataView.removeItem(draggedItem);
            event.getDropTargetItem().ifPresentOrElse(targetDefinition -> {
                if (event.getDropLocation() == GridDropLocation.BELOW) {
                    currentDataView.addItemAfter(draggedItem, targetDefinition);
                } else {
                    currentDataView.addItemBefore(draggedItem, targetDefinition);
                }
            }, () -> currentDataView.addItem(draggedItem));
            workflowTemplateEventHandler.onDefinitionUpdated(workflowTemplate.getId(), currentDataView.getItems());
        });
        availableWorkflowDefinitionsGrid.addDropListener(event -> {
            currentDataView.removeItem(draggedItem);
            availableDataView.addItem(draggedItem);
            workflowTemplateEventHandler.onDefinitionUpdated(workflowTemplate.getId(), currentDataView.getItems());
        });

        container = new Div(currentWorkflowDefinitionsGrid, availableWorkflowDefinitionsGrid);
        setContainerStyles(container);
    }

    private Grid<WorkflowDefinition> setupGrid(String footerText, GridDropMode dropMode) {
        final var grid = new Grid<>(WorkflowDefinition.class, false);
        final var idColumn = grid.addColumn(WorkflowDefinition::getId)
            .setHeader(getTranslation("workflow-definitions.main-grid.id.column"))
            .setAutoWidth(true);
        final var nameColumn = grid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-definitions.main-grid.name.column"))
            .setAutoWidth(true);
        setGridStyles(grid);
        grid.prependHeaderRow().join(idColumn, nameColumn).setText(footerText);

        grid.setDropMode(dropMode);
        grid.setRowsDraggable(true);
        grid.addDragStartListener(this::handleDragStart);
        grid.addDragEndListener(this::handleDragEnd);
        return grid;
    }

    private Component renderName(WorkflowDefinition workflowDefinition) {
        final var routerLink = new RouterLink(workflowDefinition.getName(), WorkflowDevelopmentView.class, workflowDefinition.getId());
        routerLink.getElement().getThemeList().add("badge");
        return routerLink;
    }

    private void handleDragStart(GridDragStartEvent<WorkflowDefinition> event) {
        draggedItem = event.getDraggedItems().get(0);
    }

    private void handleDragEnd(GridDragEndEvent<WorkflowDefinition> event) {
        draggedItem = null;
    }

    private static void setGridStyles(Grid<WorkflowDefinition> grid) {
        grid.getStyle().set("height", "250px")
            .set("margin-left", "0.5rem").set("margin-top", "0.5rem")
            .set("align-self", "unset");
    }

    private static void setContainerStyles(Div container) {
        container.getStyle().set("width", "100%").set("display", "flex")
            .set("flex-direction", "row").set("flex-wrap", "wrap");
    }

}
