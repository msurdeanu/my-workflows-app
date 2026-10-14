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
import org.myworkflows.util.LangUtil;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.transformer.WorkflowPrintValue;
import org.myworkflows.view.transformer.WorkflowPrintValue.Source;
import org.myworkflows.view.transformer.WorkflowPrintValue.Type;
import org.myworkflows.view.transformer.WorkflowRunPrintToValueTransformer;
import org.myworkflows.view.util.ClipboardUtil;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import static org.springframework.web.util.HtmlUtils.htmlEscape;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor
public final class WorkflowPrintGrid extends ResizableComposite<VerticalLayout> {

    private final StandardPaginatedGrid<WorkflowRunPrint, ?> paginatedGrid = new StandardPaginatedGrid<>(GridVariant.LUMO_WRAP_CELL_CONTENT);

    private final WorkflowRunPrintToValueTransformer valueTransformer = new WorkflowRunPrintToValueTransformer();

    /**
     * What every print on display was read as. A print is read once, when its row is drawn for the first time, and
     * is found again by itself: two prints of the same value are still two rows.
     */
    private final Map<WorkflowRunPrint, WorkflowPrintValue> values = new IdentityHashMap<>();

    public WorkflowPrintGrid(List<WorkflowRunPrint> prints) {
        setItems(prints);
    }

    public void setItems(List<WorkflowRunPrint> prints) {
        values.clear();
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
                htmlEscape(item.name()), htmlEscape(summarize(item)))))
            .setHeader(getTranslation("workflow-print.grid.name-value.column"));
        final var nameColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-print.grid.name.column"));
        final var valueColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValueAndType))
            .setHeader(getTranslation("workflow-print.grid.value.column"));
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(WorkflowPrintViewer::new,
            (viewer, item) -> viewer.setPrint(item.name(), item.type(), valueOf(item))));

        layout.add(paginatedGrid);

        final var contextMenu = paginatedGrid.addContextMenu();
        // on a phone a tap opens this menu instead of the details, so the details have to be reachable from here
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.details"),
            event -> event.getItem().ifPresent(it -> paginatedGrid.setDetailsVisible(it, !paginatedGrid.isDetailsVisible(it))));
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
            event -> event.getItem().ifPresent(it -> ClipboardUtil.copyTo(getElement(), valueOf(it).text())));

        paginatedGrid.setNarrowColumn(narrowColumn, List.of(nameColumn, valueColumn));
        return layout;
    }

    private Component renderName(WorkflowRunPrint print) {
        return new Badge(print.name());
    }

    private Component renderValueAndType(WorkflowRunPrint print) {
        final var span = new Badge(summarize(print));
        Tooltip.forComponent(span)
            .withText(getTranslation("workflow-print.grid.type.tooltip", print.type()))
            .withPosition(Tooltip.TooltipPosition.TOP);
        return span;
    }

    private WorkflowPrintValue valueOf(WorkflowRunPrint print) {
        return values.computeIfAbsent(print, valueTransformer::transform);
    }

    /**
     * @return what a row says about its value: the start of a text, and what a value which is more than a text is and
     *     how much it holds, as the start of such a value says little about it.
     */
    private String summarize(WorkflowRunPrint print) {
        final var value = valueOf(print);
        if (value.source() == Source.TEXT) {
            return print.abbrValue();
        } else if (value.source() == Source.TABLE) {
            return getTranslation("workflow-print.summary.table", count("row", value.totalRows()), count("column", value.columns().size()));
        }
        final var list = value.root().type() == Type.LIST;
        return getTranslation("workflow-print.summary." + (value.source() == Source.JSON ? "json-" : "") + (list ? "list" : "map"),
            count(list ? "item" : "key", value.root().children().size()));
    }

    private String count(String word, int count) {
        final var translatedWord = getTranslation("workflow-print.count." + word);
        return LangUtil.pluralize(translatedWord, count).orElseGet(() -> getTranslation("workflow-print.count.none", translatedWord));
    }

}
