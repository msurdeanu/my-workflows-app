package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Html;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.WorkflowRunPrint;
import org.myworkflows.view.component.html.SpanBadge;
import org.myworkflows.view.component.html.StandardPaginatedGrid;

import java.util.List;

import static com.vaadin.flow.component.UI.getCurrent;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor
public final class WorkflowPrintGrid extends ResizableComposite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowRunPrint, ?> paginatedGrid = new StandardPaginatedGrid<>(GridVariant.LUMO_COMPACT);

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
            paginatedGrid.setDetailsVisible(prints.getFirst(), true);
        }
    }

    @Override
    public void onSmallWidth() {
        final var visibleColumns = new boolean[]{true, false, false};
        for (int index = 0; index < visibleColumns.length; index++) {
            paginatedGrid.getColumns().get(index).setVisible(visibleColumns[index]);
        }
    }

    @Override
    public void onBigWidth() {
        final var visibleColumns = new boolean[]{false, true, true};
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
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(WorkflowPrintDetailsFormLayout::new,
            WorkflowPrintDetailsFormLayout::setExecutionPrint));

        layout.add(paginatedGrid);

        final var contextMenu = paginatedGrid.addContextMenu();
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
            event -> event.getItem().ifPresent(it -> getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", it.fullValue())));

        return layout;
    }

    private Component renderName(WorkflowRunPrint print) {
        return new SpanBadge(print.name());
    }

    private Component renderValueAndType(WorkflowRunPrint print) {
        final var span = new SpanBadge(print.abbrValue());
        Tooltip.forComponent(span)
            .withText(getTranslation("workflow-print.grid.type.tooltip", print.type()))
            .withPosition(Tooltip.TooltipPosition.TOP);
        return span;
    }

}
