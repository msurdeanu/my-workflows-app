package org.myworkflows.view;

import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.Parameter;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionEventHandler;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.repository.ParameterRepository;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowDefinitionGrid;

import java.util.stream.Stream;

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

    public WorkflowDefinitionView(WorkflowDefinitionService workflowDefinitionService,
                                  ParameterRepository parameterRepository) {
        super();
        this.workflowDefinitionService = workflowDefinitionService;

        final ConfigurableFilterDataProvider<WorkflowDefinition, Void, WorkflowDefinitionFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowDefinitionService::findBy, workflowDefinitionService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowDefinitionFilter);

        workflowDefinitionGrid = new WorkflowDefinitionGrid(this, parameterRepository.findAll());
        workflowDefinitionGrid.setDataProvider(configurableFilterDataProvider);

        add(createHeader(getTranslation("workflow-definitions.page.title")));
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
    public void onDelete(Integer workflowDefinitionId) {
        // TODO
    }

    @Override
    public void onNameUpdated(Integer workflowDefinitionId, String newName) {
        workflowDefinitionService.updateName(workflowDefinitionId, newName);
    }

    @Override
    public void onParameterUpdated(Integer workflowDefinitionId, Stream<Parameter> items) {
        workflowDefinitionService.updateParameter(workflowDefinitionId, items);
    }

    @Override
    public void onScriptUpdated(Integer workflowDefinitionId, String newScript) {
        workflowDefinitionService.updateDefinition(workflowDefinitionId, newScript);
    }

    private void onFilterById(int value) {
        workflowDefinitionFilter.setByIdCriteria(value);
        workflowDefinitionGrid.refreshPage();
    }

}
