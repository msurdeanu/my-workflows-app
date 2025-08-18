package org.myworkflows.view.component.html;

import com.vaadin.flow.component.grid.GridVariant;
import org.vaadin.klaudeta.PaginatedGrid;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public final class StandardPaginatedGrid<T, F> extends PaginatedGrid<T, F> {

    public StandardPaginatedGrid() {
        this(GridVariant.LUMO_COMPACT, GridVariant.LUMO_ROW_STRIPES, GridVariant.LUMO_WRAP_CELL_CONTENT);
    }

    public StandardPaginatedGrid(GridVariant... variants) {
        setEmptyStateText(getTranslation("paginated-grid.no-result"));
        setPageSize(10);
        setPaginatorSize(5);
        addThemeVariants(variants);
    }

}
