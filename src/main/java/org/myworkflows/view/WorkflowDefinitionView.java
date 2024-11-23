package org.myworkflows.view;

import com.vaadin.flow.component.Component;
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
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.handler.WorkflowDefinitionEventHandler;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowDefinitionGrid;

import java.util.concurrent.TimeUnit;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowDefinitionView.ROUTE, layout = BaseLayout.class)
public class WorkflowDefinitionView extends ResponsiveLayout implements HasDynamicTitle, HasUrlParameter<Integer>, WorkflowDefinitionEventHandler {

    public static final String ROUTE = "workflow/definitions";

    private final WorkflowDefinitionFilter workflowDefinitionFilter = new WorkflowDefinitionFilter();

    private final WorkflowDefinitionGrid workflowDefinitionGrid;

    private final WorkflowDefinitionService workflowDefinitionService;

    public WorkflowDefinitionView(WorkflowDefinitionService workflowDefinitionService) {
        super();
        this.workflowDefinitionService = workflowDefinitionService;

        final ConfigurableFilterDataProvider<WorkflowDefinition, Void, WorkflowDefinitionFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowDefinitionService::findBy, workflowDefinitionService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowDefinitionFilter);

        workflowDefinitionGrid = new WorkflowDefinitionGrid(this);
        workflowDefinitionGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-definitions.page.title"), createFilterByName()));
        add(createContent(workflowDefinitionGrid));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-definitions"));
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter Integer id) {
        ofNullable(id).ifPresent(this::onFilterById);
    }

    @Override
    public void onCreate(String name) {
        workflowDefinitionService.create(name);
        workflowDefinitionGrid.refreshPage();
    }

    @Override
    public void onDelete(WorkflowDefinition workflowDefinition) {
        if (workflowDefinitionService.delete(workflowDefinition) > 0) {
            workflowDefinitionGrid.refreshPage();
        }
    }

    @Override
    public void onUpdate(WorkflowDefinition workflowDefinition, String newName) {
        workflowDefinitionService.updateName(workflowDefinition, newName);
    }

    private Component createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-definitions.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        workflowDefinitionFilter.setByNameCriteria(value);
        workflowDefinitionGrid.refreshPage();
    }

    private void onFilterById(int value) {
        workflowDefinitionFilter.setByIdCriteria(value);
        workflowDefinitionGrid.refreshPage();
    }

}
