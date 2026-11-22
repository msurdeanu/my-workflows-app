package org.myworkflows.view.component;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.server.streams.DownloadHandler;
import com.vaadin.flow.server.streams.DownloadResponse;
import org.myworkflows.domain.WorkflowDownload;
import org.myworkflows.util.LangUtil;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * The files a run offers to download, each with its button. It hides itself while the run offers none.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class WorkflowDownloadList extends Composite<Div> {

    /**
     * A download is always saved, never opened by the browser: an HTML file opened from the address of the application
     * would run its scripts as the user who opened it.
     */
    public static final String MEDIA_TYPE = "application/octet-stream";

    private static final int MAX_EXTENSION_LENGTH = 4;

    private final DownloadOpener downloadOpener;
    private final List<WorkflowDownload> workflowDownloads = new ArrayList<>();
    private final Span title = new Span();
    private final Div rows = new Div();

    public WorkflowDownloadList(DownloadOpener downloadOpener) {
        this.downloadOpener = downloadOpener;
        title.addClassName("workflow-downloads-title");
        rows.addClassName("workflow-downloads-rows");
        getContent().addClassName("workflow-downloads");
        getContent().add(title, rows);
        setItems(List.of());
    }

    public void setItems(List<WorkflowDownload> items) {
        workflowDownloads.clear();
        workflowDownloads.addAll(items);
        refresh();
    }

    public void add(WorkflowDownload workflowDownload) {
        workflowDownloads.add(workflowDownload);
        refresh();
    }

    /**
     * @return a link which saves the file.
     */
    private static Anchor createDownloadAnchor(DownloadOpener downloadOpener, WorkflowDownload workflowDownload) {
        return new Anchor(DownloadHandler.fromInputStream(_ -> {
            try {
                return new DownloadResponse(downloadOpener.open(workflowDownload), workflowDownload.fileName(), MEDIA_TYPE,
                    workflowDownload.size());
            } catch (IOException gone) {
                // the directory of the run was deleted in the meantime
                return DownloadResponse.error(404);
            }
        }), "");
    }

    private void refresh() {
        rows.removeAll();
        workflowDownloads.forEach(workflowDownload -> rows.add(createRow(workflowDownload)));
        title.setText(getTranslation("workflow-downloads.title", workflowDownloads.size()));
        setVisible(!workflowDownloads.isEmpty());
    }

    private Div createRow(WorkflowDownload workflowDownload) {
        final var extension = new Span(extensionOf(workflowDownload.fileName()));
        extension.addClassName("workflow-downloads-extension");
        final var name = new Span(workflowDownload.fileName());
        name.addClassName("workflow-downloads-name");
        final var details = new Span(describe(workflowDownload));
        details.addClassName("workflow-downloads-details");
        final var text = new Div(name, details);
        text.addClassName("workflow-downloads-text");

        final var button = new Button(LangUtil.describeSize(workflowDownload.size()), VaadinIcon.DOWNLOAD.create());
        button.addThemeVariants(ButtonVariant.SMALL);
        button.setTooltipText(getTranslation("workflow-downloads.button.tooltip", workflowDownload.size()));
        button.setAriaLabel(getTranslation("workflow-downloads.button.aria-label", workflowDownload.fileName()));
        final var anchor = createDownloadAnchor(downloadOpener, workflowDownload);
        anchor.add(button);

        final var row = new Div(extension, text, anchor);
        row.addClassName("workflow-downloads-row");
        return row;
    }

    private String describe(WorkflowDownload workflowDownload) {
        final var created = getTranslation("pretty.time.format", workflowDownload.created());
        if (!workflowDownload.isZip()) {
            return created;
        }
        final var files = LangUtil.pluralize(getTranslation("workflow-downloads.zip.file"), workflowDownload.zipEntries())
            .orElseGet(() -> getTranslation("workflow-downloads.zip.no-file"));
        return getTranslation("workflow-downloads.zip.details", files,
            LangUtil.describeSize(workflowDownload.zipContentSize()), created);
    }

    private static String extensionOf(String fileName) {
        final var dot = fileName.lastIndexOf('.');
        final var extension = dot < 0 ? "" : fileName.substring(dot + 1);
        return extension.isEmpty() || extension.length() > MAX_EXTENSION_LENGTH ? "FILE" : extension.toUpperCase(Locale.ROOT);
    }

    /**
     * Opens what a file holds, as a stream the caller closes.
     */
    @FunctionalInterface
    public interface DownloadOpener {

        InputStream open(WorkflowDownload workflowDownload) throws IOException;

    }

}
