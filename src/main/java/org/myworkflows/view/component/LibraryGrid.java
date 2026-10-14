package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.server.streams.DownloadHandler;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.Library;
import org.myworkflows.domain.UserRole;
import org.myworkflows.domain.handler.LibraryEventHandler;
import org.myworkflows.util.LangUtil;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.util.ClipboardUtil;

import java.io.File;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@RequiredArgsConstructor
public final class LibraryGrid extends Composite<VerticalLayout> {

    // the date of a file where the application runs, which is where the file is
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(ZoneId.systemDefault());

    // enough of a checksum to tell two files apart at a glance; the whole of it is copied on a click
    private static final int SHORT_CHECKSUM_LENGTH = 12;

    private final StandardPaginatedGrid<Library, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final boolean isLoggedAsAdmin = UserRole.ADMIN.validate();

    private final LibraryEventHandler libraryEventHandler;

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

        final var narrowColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderPhoneCell))
            .setHeader(getTranslation("libs.grid.filepath.column"));
        final var filePathColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderFilePath))
            .setHeader(getTranslation("libs.grid.filepath.column"))
            .setWidth("150px")
            .setFlexGrow(1);
        final var sizeColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderSize))
            .setHeader(getTranslation("libs.grid.size.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        final var modifiedColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderModified))
            .setHeader(getTranslation("libs.grid.modified.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        final var checksumColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderChecksum))
            .setHeader(getTranslation("libs.grid.checksum.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        final var statusColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderStatus))
            .setHeader(getTranslation("libs.grid.status.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        final var actionsColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setAutoWidth(true)
            .setFlexGrow(0);
        paginatedGrid.setNarrowColumn(narrowColumn, List.of(filePathColumn, sizeColumn, modifiedColumn, checksumColumn, statusColumn, actionsColumn));

        layout.add(paginatedGrid);
        return layout;
    }

    /**
     * A phone shows the whole library in one cell: where it is, what it brings, then what its file is and what can be
     * done with it.
     */
    private Component renderPhoneCell(Library library) {
        final var file = new HorizontalLayout(renderSize(library), renderModified(library), renderChecksum(library));
        file.addClassName("cell-badges");
        file.setAlignItems(FlexComponent.Alignment.CENTER);
        final var actions = new HorizontalLayout(renderStatus(library), renderActions(library));
        actions.addClassName("cell-badges");
        actions.setAlignItems(FlexComponent.Alignment.CENTER);
        final var layout = new VerticalLayout(renderFilePath(library), file, actions);
        layout.addClassName("phone-cell");
        layout.setPadding(false);
        layout.setSpacing(false);
        return layout;
    }

    /**
     * The path of the library and, under it, the commands the library brings - each by the name a workflow definition
     * uses as the class of a command.
     */
    private Component renderFilePath(Library library) {
        final var anchor = new Anchor(DownloadHandler.forFile(new File(library.filePath())), library.filePath());
        if (library.commands().isEmpty()) {
            return anchor;
        }

        final var commands = new HorizontalLayout(new Span(getTranslation("libs.grid.commands.label")));
        commands.addClassName("cell-badges");
        commands.setAlignItems(FlexComponent.Alignment.CENTER);
        library.commands().forEach(command -> {
            final var badge = new Badge(command);
            badge.addThemeVariants(BadgeVariant.CONTRAST, BadgeVariant.SMALL);
            commands.add(badge);
        });
        final var layout = new VerticalLayout(anchor, commands);
        layout.setPadding(false);
        layout.setSpacing(false);
        return layout;
    }

    private Component renderSize(Library library) {
        final var size = new Span(library.hasFileFacts() ? LangUtil.describeSize(library.size()) : getTranslation("libs.grid.unknown"));
        if (library.hasFileFacts()) {
            size.setTitle(getTranslation("libs.grid.size.tooltip", library.size()));
        }
        return size;
    }

    private Component renderModified(Library library) {
        return new Span(library.hasFileFacts() ? DATE_FORMATTER.format(library.modified()) : getTranslation("libs.grid.unknown"));
    }

    /**
     * The start of the checksum, as a button which copies the whole of it: a checksum is there to be compared with
     * another one, which is not done by reading 64 characters.
     */
    private Component renderChecksum(Library library) {
        if (!library.hasFileFacts()) {
            return new Span(getTranslation("libs.grid.unknown"));
        }

        final var checksum = library.checksum();
        final var copyButton = new Button(checksum.substring(0, Math.min(SHORT_CHECKSUM_LENGTH, checksum.length())), VaadinIcon.COPY.create());
        copyButton.addClassName("library-checksum");
        copyButton.addThemeVariants(ButtonVariant.SMALL, ButtonVariant.TERTIARY);
        copyButton.setTooltipText(getTranslation("libs.grid.checksum.tooltip", checksum));
        copyButton.addClickListener(_ -> {
            ClipboardUtil.copyTo(getElement(), checksum);
            Notification.show(getTranslation("libs.grid.checksum.copied.message"));
        });
        return copyButton;
    }

    private Component renderStatus(Library library) {
        if (library.isLoaded()) {
            final var badge = new Badge(getTranslation("libs.grid.status.ok"));
            badge.addThemeVariants(BadgeVariant.SUCCESS, BadgeVariant.SMALL);
            return badge;
        } else {
            final var badge = new Badge(getTranslation("libs.grid.status.nok"));
            badge.addThemeVariants(BadgeVariant.ERROR, BadgeVariant.SMALL);
            return badge;
        }
    }

    private Component renderActions(Library library) {
        final var deleteButton = new Button(VaadinIcon.TRASH.create());
        deleteButton.addThemeVariants(ButtonVariant.SMALL, ButtonVariant.ERROR);
        deleteButton.setAriaLabel(getTranslation("libs.grid.actions.button.delete.title"));
        deleteButton.setTooltipText(getTranslation("libs.grid.actions.button.delete.title"));
        if (isLoggedAsAdmin) {
            deleteButton.addClickListener(_ -> askToDelete(library));
        } else {
            deleteButton.setEnabled(false);
        }
        final var layout = new HorizontalLayout(deleteButton);
        layout.addClassName("cell-badges");
        return layout;
    }

    private void askToDelete(Library library) {
        final var dialog = new DeleteConfirmDialog(library.fileName(), _ -> libraryEventHandler.onDelete(library));
        // deleting a library does more than removing a row, so the question says what
        dialog.setText(getTranslation(library.commands().isEmpty() ? "libs.delete.dialog.text" : "libs.delete.dialog.commands.text",
            String.join(", ", library.commands())));
        dialog.open();
    }

}
