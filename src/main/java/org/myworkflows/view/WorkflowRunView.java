package org.myworkflows.view;

import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.select.Select;
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
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.service.WorkflowTemplateService;
import org.myworkflows.util.SecurityUtil;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowRunGrid;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static java.lang.Boolean.parseBoolean;
import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowRunView.ROUTE, layout = BaseLayout.class)
public class WorkflowRunView extends ResponsiveLayout implements HasDynamicTitle, HasUrlParameter<String> {

    public static final String ROUTE = "workflow/runs";

    private final WorkflowRunFilter workflowRunFilter = new WorkflowRunFilter();

    private final WorkflowRunGrid workflowRunGrid;

    private final List<WorkflowTemplate> workflowTemplates;

    public WorkflowRunView(ApplicationManager applicationManager) {
        workflowTemplates = applicationManager.getBeanOfType(WorkflowTemplateService.class).getAll().toList();

        final var workflowRunService = applicationManager.getBeanOfType(WorkflowRunService.class);

        final ConfigurableFilterDataProvider<WorkflowRun, Void, WorkflowRunFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowRunService::findBy, workflowRunService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowRunFilter);

        workflowRunGrid = new WorkflowRunGrid(workflowRunService);
        workflowRunGrid.setDataProvider(configurableFilterDataProvider);

        final var filterByCurrentUser = createFilterByCurrentUser();
        final var filterByTemplate = createFilterByTemplate();
        final var filterByRunId = createFilterByRunId();
        add(createHeader(getTranslation("workflow-runs.page.title"), filterByCurrentUser, filterByTemplate, filterByRunId));
        // without a current user there is no checkbox to untick, so the parameter is ignored
        add(createFilters(ROUTE,
            PageFilter.choice("mine", filterByCurrentUser, false, _ -> "true",
                value -> filterByCurrentUser.isVisible() && parseBoolean(value) ? true : null,
                _ -> getTranslation("workflow-runs.filter.chip.mine")),
            PageFilter.choice("template", filterByTemplate, null, template -> String.valueOf(template.getId()), this::templateOf,
                template -> getTranslation("workflow-runs.filter.chip.template", template.getName())),
            PageFilter.text("id", filterByRunId, value -> getTranslation("workflow-runs.filter.chip.id", value))));
        add(createContent(workflowRunGrid));
        add(createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-runs"));
    }

    /**
     * Kept for the links of the grid, which point to {@code workflow/runs/<run id>}.
     */
    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String runId) {
        setPathFilterValues(runId != null ? Map.of("id", runId) : Map.of());
    }

    /**
     * @return the template with the id spelled by the query parameter, or {@code null} when there is no such template.
     */
    private WorkflowTemplate templateOf(String id) {
        return workflowTemplates.stream()
            .filter(workflowTemplate -> String.valueOf(workflowTemplate.getId()).equals(id))
            .findFirst()
            .orElse(null);
    }

    private PageGuide createGuide() {
        final var runId = new Span("#3f9c1a7e");
        runId.addClassName("workflow-run-id");
        final var details = new Button(getTranslation("pretty.time.format", Instant.now().minus(Duration.ofMinutes(5))));
        details.addThemeVariants(ButtonVariant.SMALL);
        return createGuide("workflow-runs")
            .legend("id", runId)
            .legend("user", PageGuide.badge(getTranslation("workflow-runs.grid.user.scheduler"), BadgeVariant.CONTRAST))
            .legend("template", PageGuide.link(getTranslation("workflow-runs.grid.template.column")))
            .legend("pending", PageGuide.badge(getTranslation("workflow-runs.grid.status.pending"), BadgeVariant.CONTRAST))
            .legend("success", PageGuide.badge(getTranslation("workflow-runs.grid.status.success", "240 ms"), BadgeVariant.SUCCESS))
            .legend("error", PageGuide.badge(getTranslation("workflow-runs.grid.status.error", "240 ms"), BadgeVariant.ERROR))
            .legend("details", details)
            .legend("cancel", PageGuide.button(VaadinIcon.CLOSE))
            .legend("delete", PageGuide.button(VaadinIcon.TRASH))
            .legend("replay", PageGuide.button(VaadinIcon.RECYCLE))
            .steps(4)
            .note("filters")
            .note("replay")
            .note("history");
    }

    private Checkbox createFilterByCurrentUser() {
        final var currentUserId = SecurityUtil.findCurrentUserId();
        final var filterByCurrentUserCheckbox = new Checkbox(getTranslation("workflow-runs.filter.by-user.label"));
        filterByCurrentUserCheckbox.setVisible(currentUserId.isPresent());
        filterByCurrentUserCheckbox.addValueChangeListener(event -> onFilterByUser(event.getValue() ? currentUserId.orElse(null) : null));
        return filterByCurrentUserCheckbox;
    }

    private Select<WorkflowTemplate> createFilterByTemplate() {
        final var filterByTemplateSelect = new Select<WorkflowTemplate>();
        filterByTemplateSelect.setItems(workflowTemplates);
        filterByTemplateSelect.setPlaceholder(getTranslation("workflow-runs.filter.by-template.placeholder"));
        filterByTemplateSelect.setItemLabelGenerator(item -> ofNullable(item).map(WorkflowTemplate::getName)
            .orElse(getTranslation("workflow-runs.filter.by-template.all")));
        filterByTemplateSelect.setEmptySelectionAllowed(true);
        filterByTemplateSelect.setEmptySelectionCaption(getTranslation("workflow-runs.filter.by-template.all"));
        filterByTemplateSelect.addClassName("header-filter");
        filterByTemplateSelect.addValueChangeListener(event -> onFilterByTemplate(event.getValue()));
        return filterByTemplateSelect;
    }

    private TextField createFilterByRunId() {
        final var filterByIdTextField = new TextField();
        filterByIdTextField.setPlaceholder(getTranslation("workflow-runs.filter.by-id.placeholder"));
        filterByIdTextField.setClearButtonVisible(true);
        filterByIdTextField.setValueChangeMode(ValueChangeMode.LAZY);
        filterByIdTextField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        filterByIdTextField.addClassName("header-filter");
        filterByIdTextField.addValueChangeListener(event -> onFilterByRunId(event.getValue()));
        return filterByIdTextField;
    }

    private void onFilterByTemplate(WorkflowTemplate workflowTemplate) {
        workflowRunFilter.templateIdCriteria(ofNullable(workflowTemplate).map(WorkflowTemplate::getId).orElse(0));

        workflowRunGrid.refreshPage();
    }

    private void onFilterByUser(Integer userId) {
        workflowRunFilter.userIdCriteria(userId);

        workflowRunGrid.refreshPage();
    }

    private void onFilterByRunId(String value) {
        workflowRunFilter.runIdCriteria(value);

        workflowRunGrid.refreshPage();
    }

}
