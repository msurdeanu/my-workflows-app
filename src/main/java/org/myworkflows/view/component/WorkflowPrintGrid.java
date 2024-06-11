package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Html;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.WorkflowRunPrint;
import org.vaadin.klaudeta.PaginatedGrid;

import java.util.List;

import static com.vaadin.flow.component.UI.getCurrent;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class WorkflowPrintGrid extends ResizableComposite<VerticalLayout> {

    private final PaginatedGrid<WorkflowRunPrint, ?> paginatedGrid = new PaginatedGrid<>();

    public WorkflowPrintGrid(List<WorkflowRunPrint> prints) {
        setItems(prints);
    }

    public void setItems(List<WorkflowRunPrint> prints) {
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
        paginatedGrid.addComponentColumn(item -> new Html(getTranslation("workflow-print.grid.summarized.column", item.name(), item.abbrValue())))
            .setHeader(getTranslation("workflow-print.grid.name-value.column")).setVisible(false);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-print.grid.name.column"));
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValueAndType))
            .setHeader(getTranslation("workflow-print.grid.value.column"));
        paginatedGrid.setPageSize(10);
        paginatedGrid.setPaginatorSize(5);
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(WorkflowTemplateDetailsFormLayout::new,
            WorkflowTemplateDetailsFormLayout::setExecutionPrint));
        paginatedGrid.addThemeVariants(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
        layout.add(paginatedGrid);

        final var contextMenu = paginatedGrid.addContextMenu();
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
            event -> event.getItem().ifPresent(it -> getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", it.fullValue())));

        return layout;
    }

    private Component renderName(WorkflowRunPrint print) {
        final var span = new Span(print.name());
        span.getElement().getThemeList().add("badge");
        return span;
    }

    private Component renderValueAndType(WorkflowRunPrint print) {
        final var span = new Span(print.abbrValue());
        span.getElement().getThemeList().add("badge");
        Tooltip.forComponent(span)
            .withText(getTranslation("workflow-print.grid.type.tooltip", print.type()))
            .withPosition(Tooltip.TooltipPosition.TOP);
        return span;
    }

}
