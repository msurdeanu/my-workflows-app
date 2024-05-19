package org.myworkflows.view.component;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.dnd.GridDragEndEvent;
import com.vaadin.flow.component.grid.dnd.GridDragStartEvent;
import com.vaadin.flow.component.grid.dnd.GridDropMode;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowTemplate;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowTemplateDetails extends Composite<VerticalLayout> {

    private Div container = new Div();

    private final List<WorkflowDefinition> availableWorkflowDefinitions;

    private WorkflowDefinition draggedItem;

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.add(container);
        return layout;
    }

    public void setDetails(WorkflowTemplate workflowTemplate) {
        final var templateWorkflowDefinitions = workflowTemplate.getWorkflowDefinitions();

        final var currentWorkflowDefinitionsGrid = setupGrid("Current workflow definitions");
        final var currentDataView = currentWorkflowDefinitionsGrid.setItems(templateWorkflowDefinitions);
        final var availableWorkflowDefinitionsGrid = setupGrid("Available workflow definitions");
        final var availableDataView = availableWorkflowDefinitionsGrid.setItems(availableWorkflowDefinitions);

        currentWorkflowDefinitionsGrid.addDropListener(event -> {
            availableDataView.removeItem(draggedItem);
            currentDataView.addItem(draggedItem);
        });
        availableWorkflowDefinitionsGrid.addDropListener(event -> {
            currentDataView.removeItem(draggedItem);
            availableDataView.addItem(draggedItem);
        });

        container = new Div(currentWorkflowDefinitionsGrid, availableWorkflowDefinitionsGrid);
        setContainerStyles(container);
    }

    private Grid<WorkflowDefinition> setupGrid(String header) {
        final var grid = new Grid<>(WorkflowDefinition.class, false);
        grid.addColumn(WorkflowDefinition::getName).setHeader(header);
        setGridStyles(grid);

        grid.setDropMode(GridDropMode.ON_TOP_OR_BETWEEN);
        grid.setRowsDraggable(true);
        grid.addDragStartListener(this::handleDragStart);
        grid.addDragEndListener(this::handleDragEnd);
        return grid;
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
