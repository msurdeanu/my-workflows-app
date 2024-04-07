package org.myworkflows.view;

import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.auth.AnonymousAllowed;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.domain.WorkflowTemplateFilter;
import org.myworkflows.layout.BaseLayout;
import org.myworkflows.layout.ResponsiveLayout;
import org.myworkflows.service.WorkflowTemplateService;
import org.myworkflows.view.component.WorkflowTemplateGrid;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@AnonymousAllowed
@Route(value = WorkflowTemplatesView.ROUTE, layout = BaseLayout.class)
public class WorkflowTemplatesView extends ResponsiveLayout implements HasDynamicTitle, WorkflowTemplateEventHandler {

    public static final String ROUTE = "workflow/templates";

    private final WorkflowTemplateFilter workflowTemplateFilter = new WorkflowTemplateFilter();

    private final WorkflowTemplateService workflowTemplateService;

    private final WorkflowTemplateGrid workflowTemplateGrid;

    public WorkflowTemplatesView(final WorkflowTemplateService workflowTemplateService) {
        super();
        this.workflowTemplateService = workflowTemplateService;

        final ConfigurableFilterDataProvider<WorkflowTemplate, Void, WorkflowTemplateFilter> configurableFilterDataProvider = DataProvider
                .fromFilteringCallbacks(workflowTemplateService::findBy, workflowTemplateService::countBy)
                .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowTemplateFilter);

        workflowTemplateGrid = new WorkflowTemplateGrid(this);
        workflowTemplateGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-templates.page.title"), createFilterByName()));
        add(createContent(workflowTemplateGrid));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-templates"));
    }

    @Override
    public void onDefinitionChanged(final WorkflowTemplate testScenario, final String newDefinition) {
        if (workflowTemplateService.changeDefinition(testScenario, newDefinition)) {
            Notification.show("Workflow template definition changed successfully.");
        }
    }

    private TextField createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-templates.main-grid.filter.by-name.placeholder"));
        filterByNameTextField.setHelperText(getTranslation("workflow-templates.main-grid.filter.by-name.helper"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilteringByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilteringByName(final String value) {
        workflowTemplateFilter.setByNameCriteria(value.toLowerCase());

        workflowTemplateGrid.refreshPage();
    }

}
