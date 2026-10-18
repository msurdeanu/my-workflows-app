package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.html.IFrame;
import com.vaadin.flow.component.html.Pre;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import com.vaadin.flow.router.RouteConfiguration;
import com.vaadin.flow.server.streams.DownloadHandler;
import com.vaadin.flow.server.streams.DownloadResponse;
import org.myworkflows.domain.WorkflowReport;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.service.WorkflowReportService;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.util.LangUtil;
import org.myworkflows.view.WorkflowReportView;
import org.myworkflows.view.transformer.WorkflowReportToHtmlTransformer;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

import static java.util.Optional.ofNullable;

/**
 * Shows one report: what is known about it, then its text, either as it reads or as it was written. The text is shown in
 * a frame which runs no script, has no access to the page around it and cannot be left, whatever the report holds.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class WorkflowReportViewer extends Composite<Div> {

    private final Div header = new Div();
    private final Tab renderedTab = new Tab();
    private final Tab sourceTab = new Tab();
    private final Tabs tabs = new Tabs(renderedTab, sourceTab);
    private final Div body = new Div();

    private final WorkflowReportToHtmlTransformer transformer = new WorkflowReportToHtmlTransformer();

    private final WorkflowReportService workflowReportService;
    private final WorkflowRunService workflowRunService;
    private final Function<WorkflowReport, Optional<WorkflowTemplate>> templateFinder;
    private final Consumer<WorkflowReport> onDelete;

    private WorkflowReport workflowReport;
    private String content;

    public WorkflowReportViewer(WorkflowReportService workflowReportService, WorkflowRunService workflowRunService,
                                Function<WorkflowReport, Optional<WorkflowTemplate>> templateFinder, Consumer<WorkflowReport> onDelete) {
        this.workflowReportService = workflowReportService;
        this.workflowRunService = workflowRunService;
        this.templateFinder = templateFinder;
        this.onDelete = onDelete;
    }

    /**
     * @return the report on display, if there is one.
     */
    public Optional<WorkflowReport> getWorkflowReport() {
        return ofNullable(workflowReport);
    }

    public void show(WorkflowReport newWorkflowReport) {
        workflowReport = newWorkflowReport;
        content = workflowReportService.findContent(newWorkflowReport).orElse(null);
        header.removeAll();
        header.add(createTitle(), createFacts());
        tabs.setVisible(content != null);
        showBody();
    }

    public void showNothing() {
        workflowReport = null;
        content = null;
        header.removeAll();
        tabs.setVisible(false);
        body.removeAll();
        final var message = new Span(getTranslation("workflow-reports.viewer.empty.message"));
        message.addClassName("workflow-report-message");
        body.add(message);
    }

    @Override
    protected Div initContent() {
        final var layout = super.initContent();
        layout.addClassName("workflow-report-viewer");
        header.addClassName("workflow-report-header");
        renderedTab.setLabel(getTranslation("workflow-reports.viewer.rendered.tab"));
        sourceTab.setLabel(getTranslation("workflow-reports.viewer.source.tab"));
        tabs.addSelectedChangeListener(_ -> showBody());
        body.addClassName("workflow-report-body");
        layout.add(header, tabs, body);
        showNothing();
        return layout;
    }

    private void showBody() {
        body.removeAll();
        if (workflowReport == null) {
            return;
        }
        if (content == null) {
            final var message = new Span(getTranslation("workflow-reports.viewer.missing.message", workflowReport.fileName()));
            message.addClassNames("workflow-report-message", "text-error");
            body.add(message);
        } else if (tabs.getSelectedTab() == sourceTab) {
            final var source = new Pre(content);
            source.addClassName("workflow-report-source");
            body.add(source);
        } else {
            body.add(createFrame());
        }
    }

    /**
     * Nothing but links is let out of the frame: one opens in a tab of its own, which the frame does not reach either.
     */
    private Component createFrame() {
        final var frame = new IFrame();
        frame.addClassName("workflow-report-frame");
        frame.setSandbox(IFrame.SandboxType.ALLOW_POPUPS, IFrame.SandboxType.ALLOW_POPUPS_TO_ESCAPE_SANDBOX);
        frame.setSrcdoc(transformer.transform(workflowReport.format(), content));
        frame.getElement().setAttribute("title", workflowReport.title());
        frame.getElement().setAttribute("referrerpolicy", "no-referrer");
        return frame;
    }

    private Component createTitle() {
        final var title = new H4(workflowReport.title());
        final var actions = new Div(createDownload(), createCopyLink(), createDelete());
        actions.addClassName("workflow-report-actions");
        final var layout = new Div(title, actions);
        layout.addClassName("workflow-report-title-row");
        return layout;
    }

    private Component createFacts() {
        final var run = new Span(new Span(getTranslation("workflow-reports.viewer.run.label") + " "), WorkflowReportGrid.renderRunLink(workflowReport,
            this::getTranslation));
        final var template = new Span(new Span(getTranslation("workflow-reports.viewer.template.label") + " "),
            WorkflowReportGrid.renderTemplateBadge(templateFinder.apply(workflowReport), this::getTranslation));
        final var facts = new Div(WorkflowReportGrid.renderFormatBadge(workflowReport.format(), this::getTranslation), run, template);
        facts.add(new Span(getTranslation("workflow-reports.viewer.created.label", describeUser(),
            getTranslation("pretty.time.format", workflowReport.created()), LangUtil.describeSize(workflowReport.size()))));
        facts.addClassName("workflow-report-facts");
        return facts;
    }

    private String describeUser() {
        if (workflowReport.userId() == null) {
            return getTranslation(workflowReport.workflowTemplateId() != null ? "workflow-runs.grid.user.scheduler" : "workflow-runs.grid.user.unknown");
        }
        return workflowRunService.findUsername(workflowReport.userId()).orElseGet(() -> getTranslation("workflow-runs.grid.user.deleted"));
    }

    /**
     * The file is always saved, never opened by the browser: an HTML report opened from the address of the application
     * would run its scripts as the user who opened it.
     */
    private Component createDownload() {
        final var report = workflowReport;
        final var anchor = new Anchor(DownloadHandler.fromInputStream(_ -> workflowReportService.findContent(report)
            .map(text -> text.getBytes(StandardCharsets.UTF_8))
            .map(bytes -> new DownloadResponse(new ByteArrayInputStream(bytes), report.fileName(), report.format().getMediaType(), bytes.length))
            .orElseGet(() -> DownloadResponse.error(404))), "");
        final var button = new Button(VaadinIcon.DOWNLOAD.create());
        button.addThemeVariants(ButtonVariant.SMALL);
        button.setTooltipText(getTranslation("workflow-reports.viewer.download.tooltip"));
        button.setAriaLabel(getTranslation("workflow-reports.viewer.download.tooltip"));
        anchor.add(button);
        anchor.setEnabled(content != null);
        return anchor;
    }

    private Component createCopyLink() {
        final var button = new Button(VaadinIcon.LINK.create());
        button.addThemeVariants(ButtonVariant.SMALL);
        button.setTooltipText(getTranslation("workflow-reports.viewer.copy-link.tooltip"));
        button.setAriaLabel(getTranslation("workflow-reports.viewer.copy-link.tooltip"));
        final var report = workflowReport;
        button.addClickListener(_ -> {
            final var path = RouteConfiguration.forSessionScope().getUrl(WorkflowReportView.class, report.id());
            button.getElement().executeJs("navigator.clipboard.writeText(new URL($0, document.baseURI).href)", path);
            Notification.show(getTranslation("workflow-reports.viewer.copy-link.message"));
        });
        return button;
    }

    private Component createDelete() {
        final var button = new Button(VaadinIcon.TRASH.create());
        button.addThemeVariants(ButtonVariant.SMALL, ButtonVariant.ERROR);
        button.setTooltipText(getTranslation("workflow-reports.viewer.delete.tooltip"));
        button.setAriaLabel(getTranslation("workflow-reports.viewer.delete.tooltip"));
        final var report = workflowReport;
        button.addClickListener(_ -> new DeleteConfirmDialog(report.title(), _ -> onDelete.accept(report)).open());
        return button;
    }

}
