package org.myworkflows.view;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.select.Select;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.router.AfterNavigationEvent;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.router.WildcardParameter;
import com.vaadin.flow.shared.Registration;
import jakarta.annotation.security.PermitAll;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowReportFormat;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.WorkflowReportOnChangeEvent;
import org.myworkflows.domain.filter.WorkflowReportFilter;
import org.myworkflows.service.WorkflowReportService;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.service.WorkflowTemplateService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.PageFilter;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.component.WorkflowReportGrid;
import org.myworkflows.view.component.WorkflowReportViewer;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import static java.util.Optional.ofNullable;

/**
 * The reports the runs wrote, in a grid, and the one which is chosen next to it. A report can be pointed at by its path,
 * {@code workflow/reports/<run id>/<file name>}, and the reports of a run by the query parameter {@code run}.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
@PermitAll
@Route(value = WorkflowReportView.ROUTE, layout = BaseLayout.class)
public class WorkflowReportView extends ResponsiveLayout implements HasDynamicTitle, HasUrlParameter<String> {

    public static final String ROUTE = "workflow/reports";

    private final WorkflowReportFilter workflowReportFilter = new WorkflowReportFilter();

    private final WorkflowReportGrid workflowReportGrid;

    private final WorkflowReportViewer workflowReportViewer;

    private final List<WorkflowTemplate> workflowTemplates;

    private final WorkflowReportService workflowReportService;

    private final EventBroadcaster eventBroadcaster;

    private Registration workflowReportChangeRegistration;

    /**
     * The report named by the path, shown once the filters are restored: until then the grid lists what it should not.
     */
    private String requestedReportId;

    public WorkflowReportView(ApplicationManager applicationManager) {
        workflowReportService = applicationManager.getBeanOfType(WorkflowReportService.class);
        workflowTemplates = applicationManager.getBeanOfType(WorkflowTemplateService.class).getAll().toList();
        eventBroadcaster = applicationManager.getBeanOfType(EventBroadcaster.class);

        final ConfigurableFilterDataProvider<WorkflowReport, Void, WorkflowReportFilter> configurableFilterDataProvider = DataProvider
            .fromFilteringCallbacks(workflowReportService::findBy, workflowReportService::countBy)
            .withConfigurableFilter();
        configurableFilterDataProvider.setFilter(workflowReportFilter);

        workflowReportViewer = new WorkflowReportViewer(workflowReportService, applicationManager.getBeanOfType(WorkflowRunService.class),
            workflowReportService::findWorkflowTemplate, this::onDelete);
        workflowReportGrid = new WorkflowReportGrid(workflowReportService::findWorkflowTemplate, workflowReportViewer::show);
        workflowReportGrid.setDataProvider(configurableFilterDataProvider);

        final var filterByFormat = createFilterByFormat();
        final var filterByTemplate = createFilterByTemplate();
        final var filterByRunId = createTextFilter("workflow-reports.filter.by-run.placeholder", this::onFilterByRunId);
        final var filterByTitle = createTextFilter("workflow-reports.filter.by-title.placeholder", this::onFilterByTitle);
        add(createHeader(getTranslation("workflow-reports.page.title"), filterByFormat, filterByTemplate, filterByRunId, filterByTitle));
        add(createFilters(ROUTE,
            PageFilter.choice("format", filterByFormat, null, WorkflowReportFormat::getLabel, this::formatOf,
                format -> getTranslation("workflow-reports.filter.chip.format", getTranslation("workflow-reports.format." + format.getLabel()))),
            PageFilter.choice("template", filterByTemplate, null, template -> String.valueOf(template.getId()), this::templateOf,
                template -> getTranslation("workflow-reports.filter.chip.template", template.getName())),
            PageFilter.text("run", filterByRunId, value -> getTranslation("workflow-reports.filter.chip.run", value)),
            PageFilter.text("title", filterByTitle, value -> getTranslation("workflow-reports.filter.chip.title", value))));

        final var split = new Div(workflowReportGrid, workflowReportViewer);
        split.addClassName("workflow-report-split");
        add(createContent(split));
        add(createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-reports"));
    }

    /**
     * Kept for the links which point to one report, like the one its viewer copies.
     */
    @Override
    public void setParameter(BeforeEvent beforeEvent, @WildcardParameter String reportId) {
        requestedReportId = reportId.isEmpty() ? null : reportId;
    }

    @Override
    public void afterNavigation(AfterNavigationEvent afterNavigationEvent) {
        super.afterNavigation(afterNavigationEvent);
        // the report of the path, or else the newest one the filters let through, so that the page never opens empty
        ofNullable(requestedReportId).flatMap(workflowReportService::findById)
            .or(() -> workflowReportService.getAll(workflowReportFilter, 0, 1).findFirst())
            .ifPresentOrElse(workflowReport -> {
                workflowReportViewer.show(workflowReport);
                workflowReportGrid.select(workflowReport);
            }, () -> {
                if (requestedReportId != null) {
                    Notification.show(getTranslation("workflow-reports.not-found.message", requestedReportId));
                }
            });
        requestedReportId = null;
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        final var ui = attachEvent.getUI();
        // a run writes its reports while the page is open: the grid follows without a reload
        workflowReportChangeRegistration = eventBroadcaster.register(event -> {
            final var workflowReportChangeEvent = (WorkflowReportOnChangeEvent) event;
            ui.access(() -> onReportChange(workflowReportChangeEvent));
        }, WorkflowReportOnChangeEvent.class);
    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        workflowReportChangeRegistration.remove();
        super.onDetach(detachEvent);
    }

    private void onReportChange(WorkflowReportOnChangeEvent workflowReportChangeEvent) {
        workflowReportGrid.refreshCurrentPage();
        if (workflowReportChangeEvent.deleted()) {
            workflowReportViewer.getWorkflowReport()
                .filter(workflowReportChangeEvent.workflowReports()::contains)
                .ifPresent(_ -> workflowReportViewer.showNothing());
        }
    }

    private void onDelete(WorkflowReport workflowReport) {
        workflowReportService.delete(workflowReport);
        workflowReportViewer.showNothing();
        workflowReportGrid.refreshCurrentPage();
        Notification.show(getTranslation("workflow-reports.delete.done.message", workflowReport.title()));
    }

    private PageGuide createGuide() {
        final var runId = new Span("#3f9c1a7e");
        runId.addClassName("workflow-run-id");
        final var html = PageGuide.badge(getTranslation("workflow-reports.format.html"), BadgeVariant.SMALL);
        html.addClassName("workflow-report-html");
        return createGuide("workflow-reports")
            .legend("run", runId)
            .legend("template", PageGuide.link(getTranslation("workflow-reports.grid.template.column")))
            .legend("markdown", PageGuide.badge(getTranslation("workflow-reports.format.markdown"), BadgeVariant.SMALL))
            .legend("html", html)
            .legend("download", PageGuide.button(VaadinIcon.DOWNLOAD))
            .legend("link", PageGuide.button(VaadinIcon.LINK))
            .legend("delete", PageGuide.button(VaadinIcon.TRASH, ButtonVariant.ERROR))
            .steps(3)
            .note("command")
            .note("safety")
            .note("history")
            .note("directory");
    }

    private WorkflowReportFormat formatOf(String label) {
        return Arrays.stream(WorkflowReportFormat.values())
            .filter(format -> format.getLabel().equals(label))
            .findFirst()
            .orElse(null);
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

    private Select<WorkflowReportFormat> createFilterByFormat() {
        final var select = new Select<WorkflowReportFormat>();
        select.setItems(WorkflowReportFormat.values());
        select.setItemLabelGenerator(item -> ofNullable(item).map(format -> getTranslation("workflow-reports.format." + format.getLabel()))
            .orElse(getTranslation("workflow-reports.filter.by-format.all")));
        select.setEmptySelectionAllowed(true);
        select.setEmptySelectionCaption(getTranslation("workflow-reports.filter.by-format.all"));
        select.addClassName("header-filter");
        select.addValueChangeListener(event -> {
            workflowReportFilter.formatCriteria(event.getValue());
            workflowReportGrid.refreshPage();
        });
        return select;
    }

    private Select<WorkflowTemplate> createFilterByTemplate() {
        final var select = new Select<WorkflowTemplate>();
        select.setItems(workflowTemplates);
        select.setPlaceholder(getTranslation("workflow-reports.filter.by-template.placeholder"));
        select.setItemLabelGenerator(item -> ofNullable(item).map(WorkflowTemplate::getName)
            .orElse(getTranslation("workflow-reports.filter.by-template.all")));
        select.setEmptySelectionAllowed(true);
        select.setEmptySelectionCaption(getTranslation("workflow-reports.filter.by-template.all"));
        select.addClassName("header-filter");
        select.addValueChangeListener(event -> {
            workflowReportFilter.templateIdCriteria(ofNullable(event.getValue()).map(WorkflowTemplate::getId).orElse(0));
            workflowReportGrid.refreshPage();
        });
        return select;
    }

    private TextField createTextFilter(String placeholderKey, Consumer<String> onChange) {
        final var textField = new TextField();
        textField.setPlaceholder(getTranslation(placeholderKey));
        textField.setClearButtonVisible(true);
        textField.setValueChangeMode(ValueChangeMode.LAZY);
        textField.setValueChangeTimeout((int) TimeUnit.SECONDS.toMillis(1));
        textField.addClassName("header-filter");
        textField.addValueChangeListener(event -> onChange.accept(event.getValue()));
        return textField;
    }

    private void onFilterByRunId(String value) {
        workflowReportFilter.runIdCriteria(value.strip());
        workflowReportGrid.refreshPage();
    }

    private void onFilterByTitle(String value) {
        workflowReportFilter.titleCriteria(value.strip());
        workflowReportGrid.refreshPage();
    }

}
