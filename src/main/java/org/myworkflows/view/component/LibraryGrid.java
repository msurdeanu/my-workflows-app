package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.server.streams.DownloadHandler;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.Library;
import org.myworkflows.view.component.html.SpanBadge;
import org.myworkflows.view.component.html.StandardPaginatedGrid;

import java.io.File;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@RequiredArgsConstructor
public final class LibraryGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<Library, ?> paginatedGrid = new StandardPaginatedGrid<>();

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<Library, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderFilePath))
            .setHeader(getTranslation("libs.grid.filepath.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderStatus))
            .setHeader(getTranslation("libs.grid.status.column"))
            .setAutoWidth(true);

        layout.add(paginatedGrid);
        return layout;
    }

    private Component renderFilePath(Library library) {
        return new Anchor(DownloadHandler.forFile(new File(library.filePath())), library.filePath());
    }

    private Component renderStatus(Library library) {
        if (library.isLoaded()) {
            return new SpanBadge(getTranslation("libs.grid.status.ok"), "success small");
        } else {
            return new SpanBadge(getTranslation("libs.grid.status.nok"), "error small");
        }
    }

}
