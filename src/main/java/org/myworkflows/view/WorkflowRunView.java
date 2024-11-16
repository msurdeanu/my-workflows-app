package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.service.WorkflowTemplateService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowRunGrid;

import java.util.concurrent.TimeUnit;

import static java.util.Optional.ofNullable;

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

    private final ApplicationManager applicationManager;

    public WorkflowRunView(ApplicationManager applicationManager) {
        this.applicationManager = applicationManager;

        final var workflowRunService = applicationManager.getBeanOfType(WorkflowRunService.class);

        final ConfigurableFilterDataProvider<WorkflowRun, Void, WorkflowRunFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowRunService::findBy, workflowRunService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowRunFilter);

        workflowRunGrid = new WorkflowRunGrid(workflowRunService);
        workflowRunGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-runs.page.title"), createFilterByTemplate(), createFilterByRunId()));
        add(createContent(workflowRunGrid));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-runs"));
    }

    private Component createFilterByTemplate() {
        final var filterByTemplateSelect = new Select<WorkflowTemplate>();
        filterByTemplateSelect.setItems(applicationManager.getBeanOfType(WorkflowTemplateService.class)
            .getAll().toList());
        filterByTemplateSelect.setPlaceholder(getTranslation("workflow-runs.filter.by-template.placeholder"));
        filterByTemplateSelect.setItemLabelGenerator(item -> ofNullable(item).map(WorkflowTemplate::getName)
            .orElse(getTranslation("workflow-runs.filter.by-template.none")));
        filterByTemplateSelect.setEmptySelectionAllowed(true);
        filterByTemplateSelect.setEmptySelectionCaption(getTranslation("workflow-runs.filter.by-template.none"));
        filterByTemplateSelect.addValueChangeListener(event -> onFilteringByTemplate(event.getValue()));
        return filterByTemplateSelect;
    }

    private Component createFilterByRunId() {
        final var filterByIdTextField = new TextField();
        filterByIdTextField.setPlaceholder(getTranslation("workflow-runs.filter.by-id.placeholder"));
        filterByIdTextField.setClearButtonVisible(true);
        filterByIdTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByIdTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByIdTextField.addValueChangeListener(event -> onFilteringByRunId(event.getValue()));
        return filterByIdTextField;
    }

    private void onFilteringByTemplate(WorkflowTemplate workflowTemplate) {
        workflowRunFilter.setByTemplateIdCriteria(ofNullable(workflowTemplate).map(WorkflowTemplate::getId).orElse(0));

        workflowRunGrid.refreshPage();
    }

    private void onFilteringByRunId(String value) {
        workflowRunFilter.setByRunIdCriteria(value);

        workflowRunGrid.refreshPage();
    }

}
