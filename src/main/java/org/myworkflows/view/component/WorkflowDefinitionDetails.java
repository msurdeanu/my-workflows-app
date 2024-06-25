package org.myworkflows.view.component;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.dnd.GridDragEndEvent;
import com.vaadin.flow.component.grid.dnd.GridDragStartEvent;
import com.vaadin.flow.component.grid.dnd.GridDropLocation;
import com.vaadin.flow.component.grid.dnd.GridDropMode;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.Parameter;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionEventHandler;

import java.util.List;

import static com.vaadin.flow.component.grid.dnd.GridDropMode.BETWEEN;
import static com.vaadin.flow.component.grid.dnd.GridDropMode.ON_GRID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowDefinitionDetails extends Composite<VerticalLayout> {

    private Div container = new Div();

    private final WorkflowDefinitionEventHandler workflowDefinitionEventHandler;

    private Parameter draggedItem;

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.add(container);
        return layout;
    }

    public void setDetails(WorkflowDefinition workflowDefinition, List<Parameter> availableParameters) {
        final var workflowDefinitionParameters = workflowDefinition.getParameters();

        final var currentWorkflowParametersGrid = setupGrid(getTranslation("workflow-parameters.details-grid.current"), BETWEEN);
        final var currentDataView = currentWorkflowParametersGrid.setItems(workflowDefinitionParameters);
        final var availableWorkflowParametersGrid = setupGrid(getTranslation("workflow-parameters.details-grid.available"), ON_GRID);
        final var availableDataView = availableWorkflowParametersGrid.setItems(availableParameters);

        currentWorkflowParametersGrid.addDropListener(event -> {
            availableDataView.removeItem(draggedItem);
            event.getDropTargetItem().ifPresentOrElse(targetDefinition -> {
                if (event.getDropLocation() == GridDropLocation.BELOW) {
                    currentDataView.addItemAfter(draggedItem, targetDefinition);
                } else {
                    currentDataView.addItemBefore(draggedItem, targetDefinition);
                }
            }, () -> currentDataView.addItem(draggedItem));
            workflowDefinitionEventHandler.onParameterUpdated(workflowDefinition.getId(), currentDataView.getItems());
        });
        availableWorkflowParametersGrid.addDropListener(event -> {
            currentDataView.removeItem(draggedItem);
            availableDataView.addItem(draggedItem);
            workflowDefinitionEventHandler.onParameterUpdated(workflowDefinition.getId(), currentDataView.getItems());
        });

        container = new Div(currentWorkflowParametersGrid, availableWorkflowParametersGrid);
        setContainerStyles(container);
    }

    private Grid<Parameter> setupGrid(String footerText, GridDropMode dropMode) {
        final var grid = new Grid<>(Parameter.class, false);
        final var idColumn = grid.addColumn(Parameter::getName)
            .setHeader(getTranslation("workflow-parameters.details-grid.name.column"))
            .setAutoWidth(true);
        final var nameColumn = grid.addColumn(Parameter::getType)
            .setHeader(getTranslation("workflow-parameters.details-grid.type.column"))
            .setAutoWidth(true);
        setGridStyles(grid);
        grid.prependHeaderRow().join(idColumn, nameColumn).setText(footerText);

        grid.setDropMode(dropMode);
        grid.setRowsDraggable(true);
        grid.addDragStartListener(this::handleDragStart);
        grid.addDragEndListener(this::handleDragEnd);
        return grid;
    }

    private void handleDragStart(GridDragStartEvent<Parameter> event) {
        draggedItem = event.getDraggedItems().get(0);
    }

    private void handleDragEnd(GridDragEndEvent<Parameter> event) {
        draggedItem = null;
    }

    private static void setGridStyles(Grid<Parameter> grid) {
        grid.getStyle().set("height", "250px")
            .set("margin-left", "0.5rem").set("margin-top", "0.5rem")
            .set("align-self", "unset");
    }

    private static void setContainerStyles(Div container) {
        container.getStyle().set("width", "100%").set("display", "flex")
            .set("flex-direction", "row").set("flex-wrap", "wrap");
    }

}
