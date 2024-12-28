package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.RolesAllowed;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.config.BaseConfig;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.filter.WorkflowPlaceholderFilter;
import org.myworkflows.domain.handler.WorkflowPlaceholderEventHandler;
import org.myworkflows.service.WorkflowPlaceholderService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowPlaceholderGrid;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@RolesAllowed("ROLE_ADMIN")
@Route(value = WorkflowPlaceholderView.ROUTE, layout = BaseLayout.class)
public class WorkflowPlaceholderView extends ResponsiveLayout implements HasDynamicTitle, WorkflowPlaceholderEventHandler {

    public static final String ROUTE = "workflow/placeholders";

    private final WorkflowPlaceholderFilter workflowPlaceholderFilter = new WorkflowPlaceholderFilter();

    private final WorkflowPlaceholderGrid workflowPlaceholderGrid;

    private final WorkflowPlaceholderService workflowPlaceholderService;

    public WorkflowPlaceholderView(BaseConfig baseConfig, WorkflowPlaceholderService workflowPlaceholderService) {
        super();
        this.workflowPlaceholderService = workflowPlaceholderService;

        final ConfigurableFilterDataProvider<WorkflowPlaceholder, Void, WorkflowPlaceholderFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowPlaceholderService::findBy, workflowPlaceholderService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowPlaceholderFilter);

        workflowPlaceholderGrid = new WorkflowPlaceholderGrid(this);
        workflowPlaceholderGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-placeholders.page.title"), createFilterByName()));
        add(createContent(workflowPlaceholderGrid));
        add(createFooter(baseConfig));
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-placeholders"));
    }

    @Override
    public WorkflowPlaceholder onCreate(String name) {
        final var workflowPlaceholder = workflowPlaceholderService.create(WorkflowPlaceholder.of(name), true);
        workflowPlaceholderGrid.refreshPage();
        return workflowPlaceholder;
    }

    @Override
    public void onDelete(WorkflowPlaceholder workflowPlaceholder) {
        workflowPlaceholderService.delete(workflowPlaceholder);
        workflowPlaceholderGrid.refreshPage();
    }

    @Override
    public void onUpdate(WorkflowPlaceholder workflowPlaceholder) {
        workflowPlaceholderService.update(workflowPlaceholder);
    }

    private Component createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-placeholders.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        workflowPlaceholderFilter.setByNameCriteria(value);
        workflowPlaceholderGrid.refreshPage();
    }

}
