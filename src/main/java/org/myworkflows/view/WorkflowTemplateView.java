package org.myworkflows.view;

import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.theme.lumo.LumoIcon;
import jakarta.annotation.security.PermitAll;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.UserRole;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.handler.WorkflowTemplateEventHandler;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.myworkflows.service.WorkflowDefinitionService;
import org.myworkflows.service.WorkflowTemplateService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowTemplateGrid;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.commons.lang3.math.NumberUtils.toInt;

/**
 * @author Mihai Surdeanu
 * @since 1.0
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
                                WorkflowDefinitionService workflowDefinitionService,
                                WorkflowParameterRepository workflowParameterRepository) {
        super();
        this.workflowTemplateService = workflowTemplateService;

        final ConfigurableFilterDataProvider<WorkflowTemplate, Void, WorkflowTemplateFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowTemplateService::findBy, workflowTemplateService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowTemplateFilter);

        workflowDefinitionGrid = new WorkflowTemplateGrid(this, workflowDefinitionService.getAllItems().collect(Collectors.toList()),
            workflowParameterRepository.findAll());
        workflowDefinitionGrid.setDataProvider(configurableFilterDataProvider);

        final var filterByName = createFilterByName();
        add(createHeader(getTranslation("workflow-templates.page.title"), filterByName));
        // the id has no field: it comes from a link to one template, and its chip is the way back to all of them
        add(createFilters(ROUTE,
            PageFilter.text("name", filterByName, value -> getTranslation("workflow-templates.filter.chip.name", value)),
            new PageFilter("id", null,
                () -> workflowTemplateFilter.idCriteria() > 0 ? String.valueOf(workflowTemplateFilter.idCriteria()) : StringUtils.EMPTY,
                value -> onFilterById(toInt(value)), value -> getTranslation("workflow-templates.filter.chip.id", value))));
        add(createContent(workflowDefinitionGrid));
        add(createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-templates"));
    }

    /**
     * Kept for the links of the grids, which point to {@code workflow/templates/42}.
     */
    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter Integer id) {
        setPathFilterValues(id != null ? Map.of("id", String.valueOf(id)) : Map.of());
    }

    @Override
    public void onActivationChanged(WorkflowTemplate workflowTemplate) {
        workflowTemplateService.updateActivation(workflowTemplate);
        workflowDefinitionGrid.refreshPage();
    }

    @Override
    public void onCreate(String name) {
        workflowTemplateService.create(WorkflowTemplate.of(name, new ArrayList<>(), new ArrayList<>()), true);
        workflowDefinitionGrid.refreshPage();
    }

    @Override
    public void onDefinitionUpdated(WorkflowTemplate workflowTemplate, Stream<WorkflowDefinition> newDefinitions) {
        workflowTemplateService.updateDefinition(workflowTemplate, newDefinitions);
    }

    @Override
    public void onParameterUpdated(WorkflowTemplate workflowTemplate, Stream<WorkflowParameter> items) {
        workflowTemplateService.updateParameter(workflowTemplate, items);
    }

    @Override
    public void onDelete(WorkflowTemplate workflowTemplate) {
        workflowTemplateService.delete(workflowTemplate);
        workflowDefinitionGrid.refreshPage();
    }

    @Override
    public void onNameAndCronUpdated(WorkflowTemplate workflowTemplate, @NonNull String newName, String newCron) {
        workflowTemplateService.updateNameAndCron(workflowTemplate, newName, newCron);
    }

    @Override
    public void onScheduleNow(WorkflowTemplate workflowTemplate) {
        workflowTemplateService.scheduleNow(workflowTemplate);
    }

    private PageGuide createGuide() {
        return createGuide("workflow-templates")
            .legend("activation", new Checkbox(true))
            .legend("name", PageGuide.link(getTranslation("workflow-templates.grid.name.column")))
            .legend("cron", LumoIcon.CLOCK.create())
            .legend("run", PageGuide.button(VaadinIcon.START_COG), UserRole.LOGGED)
            .legend("edit", PageGuide.button(VaadinIcon.EDIT), UserRole.ADMIN)
            .legend("delete", PageGuide.button(VaadinIcon.TRASH, ButtonVariant.ERROR), UserRole.ADMIN)
            .steps(5)
            .note("cron", PageGuide.fields(
                "0", getTranslation("workflow-templates.guide.notes.cron.second"),
                "30", getTranslation("workflow-templates.guide.notes.cron.minute"),
                "6", getTranslation("workflow-templates.guide.notes.cron.hour"),
                "*", getTranslation("workflow-templates.guide.notes.cron.day-of-month"),
                "*", getTranslation("workflow-templates.guide.notes.cron.month"),
                "MON-FRI", getTranslation("workflow-templates.guide.notes.cron.day-of-week")))
            .note("parameters")
            .note("runs");
    }

    private TextField createFilterByName() {
        final var filterByNameTextField = new TextField();
        filterByNameTextField.setPlaceholder(getTranslation("workflow-templates.filter.by-name.placeholder"));
        filterByNameTextField.setClearButtonVisible(true);
        filterByNameTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByNameTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByNameTextField.addClassName("header-filter");
        filterByNameTextField.addValueChangeListener(event -> onFilterByName(event.getValue()));
        return filterByNameTextField;
    }

    private void onFilterByName(String value) {
        workflowTemplateFilter.nameCriteria(value.toLowerCase());
        workflowDefinitionGrid.refreshPage();
    }

    private void onFilterById(int value) {
        workflowTemplateFilter.idCriteria(value);
        workflowDefinitionGrid.refreshPage();
    }

}
