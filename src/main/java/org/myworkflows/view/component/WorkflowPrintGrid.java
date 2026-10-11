package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Html;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.WorkflowRunPrint;
import org.myworkflows.view.component.html.StandardPaginatedGrid;

import java.util.List;

import static com.vaadin.flow.component.UI.getCurrent;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor
public final class WorkflowPrintGrid extends ResizableComposite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowRunPrint, ?> paginatedGrid = new StandardPaginatedGrid<>(GridVariant.LUMO_WRAP_CELL_CONTENT);

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
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        final var narrowColumn = paginatedGrid.addComponentColumn(item -> new Html(getTranslation("workflow-print.grid.summarized.column",
                item.name(), item.abbrValue())))
            .setHeader(getTranslation("workflow-print.grid.name-value.column"));
        final var nameColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-print.grid.name.column"));
        final var valueColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValueAndType))
            .setHeader(getTranslation("workflow-print.grid.value.column"));
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(WorkflowPrintDetailsFormLayout::new,
            WorkflowPrintDetailsFormLayout::setExecutionPrint));

        layout.add(paginatedGrid);

        final var contextMenu = paginatedGrid.addContextMenu();
        // on a phone a tap opens this menu instead of the details, so the details have to be reachable from here
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.details"),
            event -> event.getItem().ifPresent(it -> paginatedGrid.setDetailsVisible(it, !paginatedGrid.isDetailsVisible(it))));
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
            event -> event.getItem().ifPresent(it -> getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", it.fullValue())));

        paginatedGrid.setNarrowColumn(narrowColumn, List.of(nameColumn, valueColumn));
        return layout;
    }

    private Component renderName(WorkflowRunPrint print) {
        return new Badge(print.name());
    }

    private Component renderValueAndType(WorkflowRunPrint print) {
        final var span = new Badge(print.abbrValue());
        Tooltip.forComponent(span)
            .withText(getTranslation("workflow-print.grid.type.tooltip", print.type()))
            .withPosition(Tooltip.TooltipPosition.TOP);
        return span;
    }

}
