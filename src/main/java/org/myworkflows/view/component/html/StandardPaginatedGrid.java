package org.myworkflows.view.component.html;

import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.grid.contextmenu.GridContextMenu;
import com.vaadin.flow.data.provider.Query;
import org.myworkflows.view.component.Breakpoint;
import org.myworkflows.view.component.HasResizeableWidth;
import org.vaadin.klaudeta.PaginatedGrid;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class StandardPaginatedGrid<T, F> extends PaginatedGrid<T, F> implements HasResizeableWidth {

    private final List<Column<T>> wideColumns = new ArrayList<>();

    private Column<T> narrowColumn;
    private GridContextMenu<T> contextMenu;

    public StandardPaginatedGrid() {
        this(GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
    }

    /**
     * Cells wrap their content instead of being cut off with an ellipsis, so a row stays readable when the
     * grid is narrower than its columns would like. Density is no longer asked for through a theme variant -
     * Vaadin 25 dropped the `compact` one - but is set per screen size in `styles.css` instead.
     */
    public StandardPaginatedGrid(GridVariant... variants) {
        setEmptyStateText(getTranslation("paginated-grid.no-result"));
        setPageSize(10);
        setPaginatorSize(5);
        addThemeVariants(variants);
    }

    @Override
    public GridContextMenu<T> addContextMenu() {
        contextMenu = super.addContextMenu();
        return contextMenu;
    }

    /**
     * Registers the column that stacks a whole row into a single cell. One column always fits, so on a phone
     * it replaces the columns given here instead of letting the grid scroll sideways.
     */
    public void setNarrowColumn(Column<T> column, List<Column<T>> columns) {
        narrowColumn = column;
        wideColumns.clear();
        wideColumns.addAll(columns);
        applyBreakpoint(Breakpoint.DESKTOP);
    }

    /**
     * Reads the page on display again, for a grid whose items changed without the user asking for it. Unlike
     * {@link #refreshPaginator()}, which starts over from the first page, it leaves the user where they are.
     */
    public void refreshCurrentPage() {
        final var page = getPage();
        refreshPaginator(Math.max(page, 1));
        if (page > 1 && getDataProvider().size(new Query<>()) == 0) {
            // the items became fewer and took this page with them
            refreshPaginator();
        }
    }

    @Override
    public void onWidthChange(Breakpoint breakpoint) {
        applyBreakpoint(breakpoint);
    }

    private void applyBreakpoint(Breakpoint breakpoint) {
        final var phone = breakpoint.isPhone();
        // a full paginator draws up to eleven buttons, which is wider than a phone
        setPaginatorSize(phone ? 2 : 5);
        if (narrowColumn != null) {
            narrowColumn.setVisible(phone);
            wideColumns.forEach(column -> column.setVisible(!phone));
        }
        // touch has neither hover nor right click, so a tap opens the row actions rather than the details
        if (contextMenu != null) {
            setDetailsVisibleOnClick(!phone);
            contextMenu.setOpenOnClick(phone);
        }
    }

}
