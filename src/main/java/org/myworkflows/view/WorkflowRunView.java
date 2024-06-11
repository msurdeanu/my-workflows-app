package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowRunGrid;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowRunView.ROUTE, layout = BaseLayout.class)
public class WorkflowRunView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "workflow/runs";

    private final WorkflowRunFilter workflowRunFilter = new WorkflowRunFilter();

    private final WorkflowRunGrid workflowRunGrid;

    public WorkflowRunView(WorkflowRunService workflowRunService) {
        super();

        final ConfigurableFilterDataProvider<WorkflowRun, Void, WorkflowRunFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowRunService::findBy, workflowRunService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowRunFilter);

        workflowRunGrid = new WorkflowRunGrid();
        workflowRunGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-runs.page.title"), createFilterById()));
        add(createContent(workflowRunGrid));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-runs"));
    }

    private Component createFilterById() {
        final var filterByIdTextField = new IntegerField();
        filterByIdTextField.setPlaceholder(getTranslation("workflow-runs.filter.by-id.placeholder"));
        filterByIdTextField.setHelperText(getTranslation("workflow-runs.filter.by-id.helper"));
        filterByIdTextField.setClearButtonVisible(true);
        filterByIdTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByIdTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByIdTextField.addValueChangeListener(event -> onFilteringById(event.getValue()));
        return filterByIdTextField;
    }

    private void onFilteringById(int value) {
        workflowRunFilter.setByWorkflowIdCriteria(value);

        workflowRunGrid.refreshPage();
    }

}
