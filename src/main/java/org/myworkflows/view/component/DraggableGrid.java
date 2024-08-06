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
import com.vaadin.flow.function.SerializableFunction;
import lombok.RequiredArgsConstructor;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Stream;

import static com.vaadin.flow.component.grid.dnd.GridDropMode.BETWEEN;
import static com.vaadin.flow.component.grid.dnd.GridDropMode.ON_GRID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public class DraggableGrid<K, T> extends Composite<VerticalLayout> {

    private Div container = new Div();

    private T draggedItem;

    private final String id;

    private final SerializableFunction<T, Component> firstSerializableFunction;

    private final SerializableFunction<T, Component> secondSerializableFunction;

    private final Class<T> clazz;

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.add(container);
        return layout;
    }

    public void setDetails(K element,
                           BiConsumer<K, Stream<T>> consumer,
                           Function<K, List<T>> selectedItemsFunction,
                           List<T> availableItems) {
        final var selectedItemsGrid = setupGrid(BETWEEN);
        final var selectedDataView = selectedItemsGrid.setItems(selectedItemsFunction.apply(element));
        final var availableItemsGrid = setupGrid(ON_GRID);
        final var availableDataView = availableItemsGrid.setItems(availableItems);

        selectedItemsGrid.addDropListener(event -> {
            availableDataView.removeItem(draggedItem);
            event.getDropTargetItem().ifPresentOrElse(targetDefinition -> {
                if (event.getDropLocation() == GridDropLocation.BELOW) {
                    selectedDataView.addItemAfter(draggedItem, targetDefinition);
                } else {
                    selectedDataView.addItemBefore(draggedItem, targetDefinition);
                }
            }, () -> selectedDataView.addItem(draggedItem));
            consumer.accept(element, selectedDataView.getItems());
        });
        availableItemsGrid.addDropListener(event -> {
            selectedDataView.removeItem(draggedItem);
            availableDataView.addItem(draggedItem);
            consumer.accept(element, selectedDataView.getItems());
        });

        container = new Div(selectedItemsGrid, availableItemsGrid);
        setContainerStyles(container);
    }

    private Grid<T> setupGrid(GridDropMode dropMode) {
        final var grid = new Grid<>(clazz, false);
        final var firstColumn = grid.addColumn(new ComponentRenderer<>(firstSerializableFunction))
            .setHeader(getTranslation("draggable-grid-" + id + ".first.column"))
            .setAutoWidth(true);
        final var secondColumn = grid.addColumn(new ComponentRenderer<>(secondSerializableFunction))
            .setHeader(getTranslation("draggable-grid-" + id + ".second.column"))
            .setAutoWidth(true);
        setGridStyles(grid);
        grid.prependHeaderRow().join(firstColumn, secondColumn).setText(BETWEEN == dropMode
            ? getTranslation("draggable-grid-" + id + ".selected")
            : getTranslation("draggable-grid-" + id + ".available"));

        grid.setDropMode(dropMode);
        grid.setRowsDraggable(true);
        grid.addDragStartListener(this::handleDragStart);
        grid.addDragEndListener(this::handleDragEnd);
        return grid;
    }

    private void handleDragStart(GridDragStartEvent<T> event) {
        draggedItem = event.getDraggedItems().get(0);
    }

    private void handleDragEnd(GridDragEndEvent<T> event) {
        draggedItem = null;
    }

    private void setGridStyles(Grid<?> grid) {
        grid.getStyle().set("height", "250px")
            .set("margin-left", "0.5rem").set("margin-top", "0.5rem")
            .set("align-self", "unset");
    }

    private void setContainerStyles(Div container) {
        container.getStyle().set("width", "100%").set("display", "flex")
            .set("flex-direction", "row").set("flex-wrap", "wrap");
    }

}
