package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import org.myworkflows.domain.ExecutionPrint;

import java.util.List;

import static com.vaadin.flow.component.UI.getCurrent;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowPrintGrid extends VerticalLayout {

    private final Grid<ExecutionPrint> grid = new Grid<>();

    public WorkflowPrintGrid() {
        grid.addColumn(new ComponentRenderer<>(this::renderName))
                .setHeader(getTranslation("workflow-print.grid.name.column"))
                .setWidth("20%");
        grid.addColumn(new ComponentRenderer<>(this::renderType))
                .setHeader(getTranslation("workflow-print.grid.type.column"))
                .setWidth("15%");
        grid.addColumn(new ComponentRenderer<>(this::renderValue))
                .setHeader(getTranslation("workflow-print.grid.value.column"))
                .setWidth("65%");

        final var contextMenu = grid.addContextMenu();
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
                event -> event.getItem().ifPresent(it -> getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", it.getFullValue())));

        add(grid);
    }

    public void setItems(final List<ExecutionPrint> prints) {
        grid.setItems(prints);
        final var sizeOfPrints = prints.size();
        if (sizeOfPrints > 1) {
            for (int i = 0; i < sizeOfPrints - 1; i++) {
                grid.setDetailsVisible(prints.get(i), false);
            }
            grid.setDetailsVisible(prints.get(sizeOfPrints - 1), true);
        } else if (sizeOfPrints == 1) {
            grid.setDetailsVisible(prints.get(0), true);
        }
    }

    private Component renderName(final ExecutionPrint print) {
        return new Span(print.getName());
    }

    private Component renderType(final ExecutionPrint print) {
        return new Span(print.getType());
    }

    private Component renderValue(final ExecutionPrint print) {
        return new Span(print.getAbbrValue());
    }

}
