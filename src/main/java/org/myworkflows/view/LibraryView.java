package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.component.upload.UploadVariant;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.domain.Library;
import org.myworkflows.domain.filter.LibraryFilter;
import org.myworkflows.domain.handler.LibraryEventHandler;
import org.myworkflows.service.LibraryService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.LibraryGrid;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;

import java.io.InputStream;
import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@PermitAll
@Route(value = LibraryView.ROUTE, layout = BaseLayout.class)
public class LibraryView extends ResponsiveLayout implements HasDynamicTitle, LibraryEventHandler {

    public static final String ROUTE = "libs";

    private final LibraryFilter libraryFilter = new LibraryFilter();

    private final LibraryGrid libraryGrid;

    private final LibraryService libraryService;

    public LibraryView(LibraryService libraryService) {
        super();
        this.libraryService = libraryService;

        final ConfigurableFilterDataProvider<Library, Void, LibraryFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(libraryService::findBy, libraryService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(libraryFilter);

        libraryGrid = new LibraryGrid();
        libraryGrid.setDataProvider(configurableFilterDataProvider);

        final var filterByFilePath = createFilterByFilePath();
        add(createHeader(getTranslation("libs.page.title"), createUpload(), filterByFilePath));
        add(createFilters(ROUTE,
            PageFilter.text("path", filterByFilePath, value -> getTranslation("libs.filter.chip.path", value))));
        add(createContent(libraryGrid));
        add(createGuide("libs")
            .legend("path", PageGuide.link(getTranslation("libs.grid.filepath.column")))
            .legend("loaded", PageGuide.badge(getTranslation("libs.grid.status.ok"), BadgeVariant.SUCCESS))
            .legend("not-loaded", PageGuide.badge(getTranslation("libs.grid.status.nok"), BadgeVariant.ERROR))
            .legend("upload", new Button(VaadinIcon.UPLOAD.create()))
            .steps(3)
            .note("directory")
            .note("replace")
            .note("remove"));
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.libs"));
    }

    @Override
    public void onUpload(String fileName, InputStream inputStream) {
        libraryService.upload(fileName, inputStream);
    }

    private Component createUpload() {
        final var upload = new Upload(event -> onUpload(event.getFileName(), event.getInputStream()));
        upload.addThemeVariants(UploadVariant.AURA_NO_BORDER);
        upload.setAcceptedFileTypes("application/java-archive", LibraryConfig.JAR_EXTENSION);
        upload.setMaxFiles(1);
        upload.setDropAllowed(false);
        upload.setUploadButton(createUploadButton());
        return upload;
    }

    private Component createUploadButton() {
        final var uploadButton = new Button(VaadinIcon.UPLOAD.create());
        uploadButton.setTooltipText(getTranslation("libs.upload.button.tooltip"));
        return uploadButton;
    }

    private TextField createFilterByFilePath() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("libs.filter.by-filename.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addClassName("header-filter");
        filterByNameTextField.addValueChangeListener(event -> onFilterByFilePath(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByFilePath(String value) {
        libraryFilter.filePathCriteria(value);
        libraryGrid.refreshPage();
    }

}
