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
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterEventHandler;
import org.myworkflows.domain.filter.WorkflowParameterFilter;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowParameterGrid;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@RolesAllowed("ROLE_ADMIN")
@Route(value = WorkflowParameterView.ROUTE, layout = BaseLayout.class)
public class WorkflowParameterView extends ResponsiveLayout implements HasDynamicTitle, WorkflowParameterEventHandler {

    public static final String ROUTE = "workflow/params";

    private final WorkflowParameterFilter workflowParameterFilter = new WorkflowParameterFilter();

    private final WorkflowParameterGrid workflowParameterGrid;

    private final WorkflowParameterService workflowParameterService;

    public WorkflowParameterView(WorkflowParameterService workflowParameterService) {
        super();
        this.workflowParameterService = workflowParameterService;

        final ConfigurableFilterDataProvider<WorkflowParameter, Void, WorkflowParameterFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowParameterService::findBy, workflowParameterService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowParameterFilter);

        workflowParameterGrid = new WorkflowParameterGrid(this);
        workflowParameterGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-params.page.title"), createFilterByName()));
        add(createContent(workflowParameterGrid));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-params"));
    }

    @Override
    public void onCreate(String name) {
        workflowParameterService.create(name);
        workflowParameterGrid.refreshPage();
    }

    @Override
    public void onDelete(WorkflowParameter workflowParameter) {
        workflowParameterService.delete(workflowParameter);
        workflowParameterGrid.refreshPage();
    }

    @Override
    public void onUpdate(WorkflowParameter workflowParameter) {
        workflowParameterService.update(workflowParameter);
    }

    private Component createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-params.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        workflowParameterFilter.setByNameCriteria(value);
        workflowParameterGrid.refreshPage();
    }

}
