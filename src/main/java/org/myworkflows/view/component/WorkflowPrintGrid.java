package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Html;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import org.myworkflows.domain.ExecutionPrint;
import org.vaadin.klaudeta.PaginatedGrid;

import java.util.List;

import static com.vaadin.flow.component.UI.getCurrent;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowPrintGrid extends ResizableComposite<VerticalLayout> {

    private final PaginatedGrid<ExecutionPrint, ?> paginatedGrid = new PaginatedGrid<>();

    public void setItems(final List<ExecutionPrint> prints) {
        paginatedGrid.setItems(prints);
        final var sizeOfPrints = prints.size();
        if (sizeOfPrints > 1) {
            for (int i = 0; i < sizeOfPrints - 1; i++) {
                paginatedGrid.setDetailsVisible(prints.get(i), false);
            }
            paginatedGrid.setDetailsVisible(prints.get(sizeOfPrints - 1), true);
        } else if (sizeOfPrints == 1) {
            paginatedGrid.setDetailsVisible(prints.get(0), true);
        }
    }

    @Override
    public void onSmallWidth() {
        boolean[] visibleColumns = new boolean[]{true, false, false};
        for (int index = 0; index < visibleColumns.length; index++) {
            paginatedGrid.getColumns().get(index).setVisible(visibleColumns[index]);
        }
    }

    @Override
    public void onBigWidth() {
        boolean[] visibleColumns = new boolean[]{false, true, true};
        for (int index = 0; index < visibleColumns.length; index++) {
            paginatedGrid.getColumns().get(index).setVisible(visibleColumns[index]);
        }
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        paginatedGrid.addComponentColumn(item -> new Html(getTranslation("workflow-print.grid.summarized.column", item.name(), item.getAbbrValue())))
            .setHeader(getTranslation("workflow-print.grid.name-value.column")).setVisible(false);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-print.grid.name.column"));
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValueAndType))
            .setHeader(getTranslation("workflow-print.grid.value.column"));
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        final var contextMenu = paginatedGrid.addContextMenu();
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
            event -> event.getItem().ifPresent(it -> getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", it.getFullValue())));

        return layout;
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
