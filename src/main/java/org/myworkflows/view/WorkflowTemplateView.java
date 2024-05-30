package org.myworkflows.view;

import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.service.WorkflowTemplateService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowTemplateGrid;

import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowTemplateView.ROUTE, layout = BaseLayout.class)
public class WorkflowTemplateView extends ResponsiveLayout implements HasDynamicTitle, HasUrlParameter<Integer>, WorkflowTemplateEventHandler {

    public static final String ROUTE = "workflow/templates";

    private final WorkflowTemplateFilter workflowTemplateFilter = new WorkflowTemplateFilter();

    private final WorkflowTemplateService workflowTemplateService;

    private final WorkflowTemplateGrid workflowDefinitionGrid;

    public WorkflowTemplateView(WorkflowTemplateService workflowTemplateService,
                                WorkflowDefinitionService workflowDefinitionService) {
        super();
        this.workflowTemplateService = workflowTemplateService;

        final ConfigurableFilterDataProvider<WorkflowTemplate, Void, WorkflowTemplateFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowTemplateService::findBy, workflowTemplateService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowTemplateFilter);

        workflowDefinitionGrid = new WorkflowTemplateGrid(this, workflowDefinitionService.getAllItems().collect(Collectors.toList()));
        workflowDefinitionGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-templates.page.title"), createFilterByName()));
        add(createContent(workflowDefinitionGrid));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-templates"));
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter Integer id) {
        ofNullable(id).ifPresent(this::onFilterById);
    }

    @Override
    public void onActivationChanged(Integer workflowTemplateId) {
        workflowTemplateService.updateActivation(workflowTemplateId);
        workflowDefinitionGrid.refreshPage();
    }

    @Override
    public void onDefinitionUpdated(Integer workflowTemplateId, Stream<WorkflowDefinition> newDefinitions) {
        workflowTemplateService.updateDefinition(workflowTemplateId, newDefinitions);
    }

    @Override
    public void onDelete(Integer workflowTemplateId) {
        workflowTemplateService.delete(workflowTemplateId);
        workflowDefinitionGrid.refreshPage();
    }

    @Override
    public void onNameAndCronUpdated(Integer workflowTemplateId, @NonNull String newName, String newCron) {
        workflowTemplateService.updateNameAndCron(workflowTemplateId, newName, newCron);
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
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        workflowTemplateFilter.setByNameCriteria(value.toLowerCase());
        workflowDefinitionGrid.refreshPage();
    }

    private void onFilterById(int value) {
        workflowTemplateFilter.setByIdCriteria(value);
        workflowDefinitionGrid.refreshPage();
    }

}
