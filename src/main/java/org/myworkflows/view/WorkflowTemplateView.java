package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
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
import org.myworkflows.view.component.WorkflowDuplicateDialog;
import org.myworkflows.view.component.WorkflowTemplateGrid;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.math.NumberUtils.toInt;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.definitionsOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.isOwn;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.ownNameOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.parametersOf;

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
    public void onDuplicate(WorkflowTemplate workflowTemplate) {
        final var ownParameters = parametersOf(workflowTemplate).stream().filter(parameter -> isOwn(workflowTemplate, parameter)).toList();
        final var ownParametersCheckbox = new Checkbox(getTranslation("workflow-templates.duplicate.own-parameters.checkbox"), true);
        final var dialog = new WorkflowDuplicateDialog(workflowTemplate.getName(), workflowTemplateService.suggestCopyName(workflowTemplate),
            workflowTemplateService::isNameTaken, name -> {
            final var copy = workflowTemplateService.duplicate(workflowTemplate, name, !ownParameters.isEmpty() && ownParametersCheckbox.getValue());
            Notification.show(getTranslation("workflow-templates.duplicate.success.message", copy.getName()));
            // shown alone, as the link to one template shows it, and opened: what it got is checked before it is ticked
            getUI().ifPresent(ui -> ui.navigate(WorkflowTemplateView.class, copy.getId()));
            workflowDefinitionGrid.showDetails(copy);
        });

        final var summary = new Div();
        summary.addClassName("workflow-duplicate-summary");
        addSummaryRow(summary, "definitions", definitionsOf(workflowTemplate).stream().map(definition -> createBadge(definition.getName())));
        addSummaryRow(summary, "parameters", parametersOf(workflowTemplate).stream()
            .filter(parameter -> !isOwn(workflowTemplate, parameter))
            .map(parameter -> createBadge(parameter.getName())));
        addSummaryRow(summary, "own-parameters", ownParameters.stream().map(parameter -> {
            final var renaming = new Span(createBadge(parameter.getName()), new Span("→"), createBadge(
                getTranslation("workflow-templates.duplicate.own-parameters.new-name", ownNameOf(workflowTemplate, parameter))));
            renaming.addClassName("workflow-duplicate-renaming");
            return renaming;
        }), ownParametersCheckbox);
        ofNullable(StringUtils.trimToNull(workflowTemplate.getCron())).ifPresent(cron -> {
            final var cronCode = new Span(cron);
            cronCode.addClassName("page-guide-code");
            addSummaryRow(summary, "cron", Stream.of(cronCode));
        });
        final var pausedMessage = new Div(getTranslation("workflow-templates.duplicate.paused.message"));
        pausedMessage.addClassNames("workflow-status", "paused");
        dialog.addDetails(summary, pausedMessage);
        dialog.open();
    }

    @Override
    public void onNameAndCronUpdated(WorkflowTemplate workflowTemplate, @NonNull String newName, String newCron) {
        workflowTemplateService.updateNameAndCron(workflowTemplate, newName, newCron);
    }

    @Override
    public void onScheduleNow(WorkflowTemplate workflowTemplate) {
        workflowTemplateService.scheduleNow(workflowTemplate);
    }

    /**
     * Adds a row which names something the copy takes from the original, unless the original has none of it.
     *
     * @param after what goes under the row text.
     */
    private void addSummaryRow(Div summary, String key, Stream<? extends Component> items, Component... after) {
        final List<Component> itemList = items.map(Component.class::cast).toList();
        if (itemList.isEmpty()) {
            return;
        }
        final var label = new Span(getTranslation("workflow-templates.duplicate." + key + ".label"));
        label.addClassName("workflow-duplicate-summary-label");
        final var values = new Div();
        values.addClassName("workflow-duplicate-summary-values");
        values.add(itemList);
        final var text = new Span(getTranslation("workflow-templates.duplicate." + key + ".text"));
        text.addClassName("workflow-duplicate-summary-text");
        values.add(text);
        values.add(after);
        summary.add(new Div(label, values));
    }

    private static Badge createBadge(String text) {
        final var badge = new Badge(text);
        badge.addThemeVariants(BadgeVariant.SMALL);
        return badge;
    }

    private PageGuide createGuide() {
        return createGuide("workflow-templates")
            .legend("activation", new Checkbox(true))
            .legend("name", PageGuide.link(getTranslation("workflow-templates.grid.name.column")))
            .legend("cron", LumoIcon.CLOCK.create())
            .legend("run", PageGuide.button(VaadinIcon.START_COG), UserRole.LOGGED)
            .legend("edit", PageGuide.button(VaadinIcon.EDIT), UserRole.ADMIN)
            .legend("duplicate", PageGuide.button(VaadinIcon.COPY_O), UserRole.ADMIN)
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
