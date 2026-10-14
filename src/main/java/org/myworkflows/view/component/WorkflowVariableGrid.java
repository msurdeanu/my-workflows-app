package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Html;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.textfield.TextFieldVariant;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.data.value.ValueChangeMode;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowRunVariable;
import org.myworkflows.view.component.html.StandardPaginatedGrid;

import java.util.Comparator;
import java.util.List;

import static com.vaadin.flow.component.UI.getCurrent;
import static org.springframework.web.util.HtmlUtils.htmlEscape;

/**
 * Everything a workflow run held the last time it stood still: when it paused before a command, or when it ended. What
 * the run added or changed since the time before comes first, with a badge which says so.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowVariableGrid extends ResizableComposite<VerticalLayout> {

    private static final int MAX_VALUE_LENGTH = 48;

    private final StandardPaginatedGrid<WorkflowRunVariable, ?> paginatedGrid = new StandardPaginatedGrid<>(GridVariant.LUMO_WRAP_CELL_CONTENT);

    private final TextField filterByName = new TextField();

    private List<WorkflowRunVariable> variables = List.of();

    public void setItems(List<WorkflowRunVariable> newVariables) {
        // the names are already sorted: a stable sort only brings forward what has a badge
        variables = newVariables.stream()
            .sorted(Comparator.comparing(variable -> !variable.isMarked()))
            .toList();
        showItems();
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        filterByName.setPlaceholder(getTranslation("workflow-variable.grid.filter.placeholder"));
        filterByName.setAriaLabel(getTranslation("workflow-variable.grid.filter.placeholder"));
        filterByName.addThemeVariants(TextFieldVariant.SMALL);
        filterByName.setClearButtonVisible(true);
        filterByName.setWidthFull();
        filterByName.setValueChangeMode(ValueChangeMode.LAZY);
        filterByName.addValueChangeListener(_ -> showItems());

        paginatedGrid.setEmptyStateText(getTranslation("workflow-variable.grid.empty"));
        final var narrowColumn = paginatedGrid.addComponentColumn(this::renderPhoneCell)
            .setHeader(getTranslation("workflow-print.grid.name-value.column"));
        final var nameColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderName))
            .setHeader(getTranslation("workflow-print.grid.name.column"));
        final var valueColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderValueAndType))
            .setHeader(getTranslation("workflow-print.grid.value.column"));
        paginatedGrid.setItemDetailsRenderer(new ComponentRenderer<>(WorkflowPrintDetailsFormLayout::new,
            WorkflowPrintDetailsFormLayout::setVariable));

        final var contextMenu = paginatedGrid.addContextMenu();
        // on a phone a tap opens this menu instead of the details, so the details have to be reachable from here
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.details"),
            event -> event.getItem().ifPresent(it -> paginatedGrid.setDetailsVisible(it, !paginatedGrid.isDetailsVisible(it))));
        contextMenu.addItem(getTranslation("workflow-print.grid.context-menu.value-to-clipboard"),
            event -> event.getItem().ifPresent(it -> getCurrent().getPage().executeJs("navigator.clipboard.writeText($0)", it.value())));

        paginatedGrid.setNarrowColumn(narrowColumn, List.of(nameColumn, valueColumn));
        layout.add(filterByName, paginatedGrid);
        return layout;
    }

    private void showItems() {
        final var filter = filterByName.getValue().trim();
        paginatedGrid.setItems(variables.stream()
            .filter(variable -> StringUtils.containsIgnoreCase(variable.name(), filter))
            .toList());
    }

    private Component renderPhoneCell(WorkflowRunVariable variable) {
        final var layout = new HorizontalLayout(new Html(getTranslation("workflow-print.grid.summarized.column",
            htmlEscape(variable.name()), htmlEscape(abbreviate(variable)))));
        layout.addClassName("cell-badges");
        addChangeBadge(layout, variable);
        return layout;
    }

    private Component renderName(WorkflowRunVariable variable) {
        final var layout = new HorizontalLayout(new Badge(variable.name()));
        layout.addClassName("cell-badges");
        addChangeBadge(layout, variable);
        return layout;
    }

    private Component renderValueAndType(WorkflowRunVariable variable) {
        final var badge = new Badge(abbreviate(variable));
        Tooltip.forComponent(badge)
            .withText(getTranslation("workflow-print.grid.type.tooltip", variable.type()))
            .withPosition(Tooltip.TooltipPosition.TOP);
        return badge;
    }

    private void addChangeBadge(HorizontalLayout layout, WorkflowRunVariable variable) {
        if (!variable.isMarked()) {
            return;
        }
        final var added = variable.change() == WorkflowRunVariable.Change.ADDED;
        final var badge = new Badge(getTranslation(added ? "workflow-variable.grid.added" : "workflow-variable.grid.changed"));
        badge.addThemeVariants(added ? BadgeVariant.SUCCESS : BadgeVariant.WARNING, BadgeVariant.SMALL);
        layout.add(badge);
    }

    private String abbreviate(WorkflowRunVariable variable) {
        return StringUtils.abbreviate(variable.value(), MAX_VALUE_LENGTH);
    }

}
