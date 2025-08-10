package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.config.BaseConfig;
import org.myworkflows.config.LibraryConfig;
import org.myworkflows.domain.Library;
import org.myworkflows.domain.filter.LibraryFilter;
import org.myworkflows.domain.handler.LibraryEventHandler;
import org.myworkflows.service.LibraryService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.LibraryGrid;
import org.myworkflows.view.component.ResponsiveLayout;

import java.io.InputStream;
import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = LibraryView.ROUTE, layout = BaseLayout.class)
public class LibraryView extends ResponsiveLayout implements HasDynamicTitle, LibraryEventHandler {

    public static final String ROUTE = "libs";

    private final LibraryFilter libraryFilter = new LibraryFilter();

    private final LibraryGrid libraryGrid;

    private final LibraryService libraryService;

    public LibraryView(BaseConfig baseConfig, LibraryService libraryService) {
        super();
        this.libraryService = libraryService;

        final ConfigurableFilterDataProvider<Library, Void, LibraryFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(libraryService::findBy, libraryService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(libraryFilter);

        libraryGrid = new LibraryGrid();
        libraryGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("libs.page.title"), createUpload(), createFilterByFilePath()));
        add(createContent(libraryGrid));
        add(createFooter(baseConfig));
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
        upload.setAcceptedFileTypes("application/java-archive", LibraryConfig.JAR_EXTENSION);
        upload.setMaxFiles(1);
        upload.setDropAllowed(false);
        return upload;
    }

    private Component createFilterByFilePath() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("libs.filter.by-filename.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilterByFilePath(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByFilePath(String value) {
        libraryFilter.filePathCriteria(value);
        libraryGrid.refreshPage();
    }

}
