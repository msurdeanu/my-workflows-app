package org.myworkflows.view.component.html;

import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.provider.Query;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class StandardPaginatedGridTest {

    @Test
    public void testRefreshCurrentPageKeepsThePageOnDisplay() {
        // given a user on the third page
        final var items = itemsUpTo(35);
        final var grid = createGrid(items);
        grid.setPage(3);

        // when an item comes in at the top
        items.addFirst(-1);
        grid.refreshCurrentPage();

        // then the third page is read again, moved by that item
        assertEquals(3, grid.getPage());
        assertEquals(IntStream.range(19, 29).boxed().toList(), displayedItems(grid));
    }

    @Test
    public void testRefreshCurrentPageGoesBackToTheFirstPageWhenThePageOnDisplayIsGone() {
        // given a user on the third page
        final var items = itemsUpTo(25);
        final var grid = createGrid(items);
        grid.setPage(3);

        // when the items of that page are gone
        items.subList(15, 25).clear();
        grid.refreshCurrentPage();

        // then
        assertEquals(1, grid.getPage());
        assertEquals(IntStream.range(0, 10).boxed().toList(), displayedItems(grid));
    }

    @Test
    public void testRefreshCurrentPageReadsTheFirstPageOfAGridThatWasNeverPaged() {
        // given
        final var items = itemsUpTo(5);
        final var grid = createGrid(items);

        // when
        items.addFirst(-1);
        grid.refreshCurrentPage();

        // then
        assertEquals(1, grid.getPage());
        assertEquals(IntStream.range(-1, 5).boxed().toList(), displayedItems(grid));
    }

    private static List<Integer> itemsUpTo(int size) {
        return new ArrayList<>(IntStream.range(0, size).boxed().toList());
    }

    private static StandardPaginatedGrid<Integer, Void> createGrid(List<Integer> items) {
        final var grid = new StandardPaginatedGrid<Integer, Void>();
        grid.setDataProvider(DataProvider.fromCallbacks(query -> items.stream().skip(query.getOffset()).limit(query.getLimit()),
            _ -> items.size()));
        return grid;
    }

    private static List<Integer> displayedItems(StandardPaginatedGrid<Integer, Void> grid) {
        return grid.getDataProvider().fetch(new Query<>()).toList();
    }

}
