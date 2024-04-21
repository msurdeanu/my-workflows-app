package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Html;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import org.myworkflows.domain.ExecutionPrint;

import java.util.List;

import static com.vaadin.flow.component.UI.getCurrent;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowPrintGrid extends AdjustableWidthComposite<VerticalLayout> {

    private final Grid<ExecutionPrint> grid = new Grid<>();

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

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        grid.addComponentColumn(item -> new Html(getTranslation("workflow-print.grid.summarized.column", item.name(), item.getAbbrValue())))
            .setHeader(getTranslation("workflow-print.grid.name.column")).setVisible(false);
        grid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-print.grid.name.column"));
        grid.addColumn(new ComponentRenderer<>(this::renderValueAndType))
            .setHeader(getTranslation("workflow-print.grid.value.column"));
        layout.add(grid);

        final var contextMenu = grid.addContextMenu();
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
            event -> event.getItem().ifPresent(it -> getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", it.getFullValue())));

        return layout;
    }

    @Override
    protected void adjustByWidth(final int width) {
        boolean[] visibleColumns;
        if (width > THRESHOLD_WIDTH) {
            visibleColumns = new boolean[]{false, true, true};
        } else {
            visibleColumns = new boolean[]{true, false, false};
        }
        for (int index = 0; index < visibleColumns.length; index++) {
            grid.getColumns().get(index).setVisible(visibleColumns[index]);
        }
    }

    private Component renderName(final ExecutionPrint print) {
        final var span = new Span(print.name());
        span.getElement().getThemeList().add("badge");
        return span;
    }

    private Component renderValueAndType(final ExecutionPrint print) {
        final var span = new Span(print.getAbbrValue());
        span.getElement().getThemeList().add("badge");
        Tooltip.forComponent(span)
            .withText(getTranslation("workflow-print.grid.type.tooltip", print.getType()))
            .withPosition(Tooltip.TooltipPosition.TOP);
        return span;
    }

}
