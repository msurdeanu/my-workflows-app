package org.myworkflows.view;

import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.domain.WorkflowTemplateFilter;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.service.WorkflowTemplateService;
import org.myworkflows.view.component.WorkflowTemplateGrid;

import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowTemplatesView.ROUTE, layout = BaseLayout.class)
public class WorkflowTemplatesView extends ResponsiveLayout implements HasDynamicTitle, WorkflowTemplateEventHandler {

    public static final String ROUTE = "workflow/templates";

    private final WorkflowTemplateFilter workflowTemplateFilter = new WorkflowTemplateFilter();

    private final WorkflowTemplateService workflowTemplateService;

    private final WorkflowTemplateGrid workflowTemplateGrid;

    public WorkflowTemplatesView(WorkflowTemplateService workflowTemplateService) {
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
    public void onActivationChanged(Integer workflowTemplateId) {
        workflowTemplateService.changeActivation(workflowTemplateId);
        workflowTemplateGrid.refreshPage();
    }

    @Override
    public void onCronChanged(Integer workflowTemplateId, String newCron) {
        workflowTemplateService.changeCron(workflowTemplateId, newCron);
    }

    @Override
    public void onDefinitionChanged(Integer workflowTemplateId, String newDefinition) {
        if (workflowTemplateService.changeDefinition(workflowTemplateId, newDefinition)) {
            Notification.show("Workflow template definition changed successfully.");
        }
    }

    @Override
    public void onDelete(Integer workflowTemplateId) {
        workflowTemplateService.delete(workflowTemplateId);
        workflowTemplateGrid.refreshPage();
    }

    @Override
    public void onNameChanged(Integer workflowTemplateId, @NonNull String newName) {
        if (workflowTemplateService.changeName(workflowTemplateId, newName)) {
            Notification.show("Workflow template name changed successfully to '" + newName + "'.");
        }
    }

    @Override
    public void onScheduleNow(Integer workflowTemplateId) {
        workflowTemplateService.scheduleNow(workflowTemplateId);
    }

    private TextField createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-templates.filter.by-name.placeholder"));
        filterByNameTextField.setHelperText(getTranslation("workflow-templates.filter.by-name.helper"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setMinWidth("200px");
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilteringByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilteringByName(String value) {
        workflowTemplateFilter.setByNameCriteria(value.toLowerCase());

        workflowTemplateGrid.refreshPage();
    }

}
