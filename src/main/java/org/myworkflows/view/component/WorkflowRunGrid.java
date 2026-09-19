package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.popover.Popover;
import com.vaadin.flow.component.popover.PopoverPosition;
import com.vaadin.flow.component.popover.PopoverVariant;
import com.vaadin.flow.component.shared.Tooltip;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.router.RouterLink;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.view.WorkflowRunView;
import org.myworkflows.view.WorkflowTemplateView;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.util.ClipboardUtil;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@RequiredArgsConstructor
public final class WorkflowRunGrid extends Composite<VerticalLayout> {

    private static final int SHORT_ID_LENGTH = 8;

    private final StandardPaginatedGrid<WorkflowRun, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final WorkflowRunService workflowRunService;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowRun, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderId))
            .setHeader(getTranslation("workflow-runs.grid.id.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderTemplateId))
            .setHeader(getTranslation("workflow-runs.grid.template.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderStatus))
            .setHeader(getTranslation("workflow-runs.grid.status.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderDetails))
            .setHeader(getTranslation("workflow-runs.grid.details.column"))
            .setAutoWidth(true);
        paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(getTranslation("workflow-runs.grid.actions.column"))
            .setAutoWidth(true);

        layout.add(paginatedGrid);
        return layout;
    }

    private Component renderId(WorkflowRun workflowRun) {
        final var workflowIdAsString = workflowRun.getId().toString();
        // The link still targets the full id; only the label is shortened, the full id being available on hover.
        final var routerLink = new RouterLink("#" + workflowIdAsString.substring(0, SHORT_ID_LENGTH),
            WorkflowRunView.class, workflowIdAsString);
        routerLink.addClassName("workflow-run-id");
        Tooltip.forComponent(routerLink).setText(workflowIdAsString);

        final var copyButton = new Button(VaadinIcon.COPY_O.create());
        copyButton.addThemeVariants(ButtonVariant.TERTIARY, ButtonVariant.SMALL);
        copyButton.setTooltipText(getTranslation("workflow-runs.grid.id.copy.tooltip"));
        copyButton.setAriaLabel(getTranslation("workflow-runs.grid.id.copy.tooltip"));
        copyButton.addClickListener(_ -> {
            ClipboardUtil.copyTo(copyButton.getElement(), workflowIdAsString);
            Notification.show(getTranslation("workflow-runs.grid.id.copy.message"));
        });

        final var layout = new HorizontalLayout(routerLink, copyButton);
        layout.setSpacing(false);
        layout.setAlignItems(FlexComponent.Alignment.CENTER);
        return layout;
    }

    private Component renderTemplateId(WorkflowRun workflowRun) {
        return workflowRunService.findWorkflowTemplate(workflowRun).map(template -> {
            final var routerLink = new RouterLink(template.getName(), WorkflowTemplateView.class, template.getId());
            routerLink.getElement().getThemeList().add("badge small");
            return (Component) routerLink;
        }).orElseGet(() -> {
            final var badge = new Badge(getTranslation("workflow-runs.grid.template.manual"));
            badge.addThemeVariants(BadgeVariant.CONTRAST, BadgeVariant.SMALL);
            return badge;
        });
    }

    private Component renderStatus(WorkflowRun workflowRun) {
        if (workflowRun.isRunning()) {
            final var badge = new Badge(getTranslation("workflow-runs.grid.status.pending", workflowRun.getHumanReadableDuration()));
            badge.addThemeVariants(BadgeVariant.CONTRAST, BadgeVariant.SMALL);
            return badge;
        }

        return ofNullable(workflowRun.getFailureMessage())
            .map(_ -> {
                final var badge = new Badge(getTranslation("workflow-runs.grid.status.error", workflowRun.getHumanReadableDuration()));
                badge.addThemeVariants(BadgeVariant.ERROR, BadgeVariant.SMALL);
                final var popover = new Popover();
                popover.setTarget(badge);
                popover.setWidth("300px");
                popover.addThemeVariants(PopoverVariant.ARROW);
                popover.setPosition(PopoverPosition.BOTTOM);
                popover.add(createExceptionBlock(workflowRun));
                return badge;
            })
            .orElseGet(() -> {
                final var badge = new Badge(getTranslation("workflow-runs.grid.status.success", workflowRun.getHumanReadableDuration()));
                badge.addThemeVariants(BadgeVariant.SUCCESS, BadgeVariant.SMALL);
                return badge;
            });
    }

    private Component renderDetails(WorkflowRun workflowRun) {
        final var button = new Button(getTranslation("pretty.time.format", workflowRun.getCreated()));
        button.addThemeVariants(ButtonVariant.SMALL);
        button.setTooltipText(workflowRun.getCreated().toString());
        button.addClickListener(_ -> new WorkflowRunDetailsDialog(workflowRun).open());
        return button;
    }

    private Component renderActions(WorkflowRun workflowRun) {
        final var layout = new HorizontalLayout();
        if (workflowRun.isRunning()) {
            final var cancelButton = new Button(VaadinIcon.CLOSE.create());
            cancelButton.addThemeVariants(ButtonVariant.SMALL);
            cancelButton.addClickListener(_ -> cancel(workflowRun));
            layout.add(cancelButton);
        } else {
            final var deleteButton = new Button(VaadinIcon.TRASH.create());
            deleteButton.addThemeVariants(ButtonVariant.SMALL);
            deleteButton.addClickListener(_ -> delete(workflowRun));
            layout.add(deleteButton);
            if (workflowRun.isEligibleForReplay()) {
                final var replayButton = new Button(VaadinIcon.RECYCLE.create());
                replayButton.addThemeVariants(ButtonVariant.SMALL);
                replayButton.addClickListener(_ -> replay(workflowRun));
                layout.add(replayButton);
            }
        }
        return layout;
    }

    private void cancel(WorkflowRun workflowRun) {
        if (workflowRun.cancelAndInterruptIfRunning()) {
            Notification.show(getTranslation("workflow-runs.grid.run.cancelled"));
            paginatedGrid.getDataProvider().refreshItem(workflowRun);
        }
    }

    private void delete(WorkflowRun workflowRun) {
        workflowRunService.delete(workflowRun);
        refreshPage();
    }


    private void replay(WorkflowRun workflowRun) {
        workflowRunService.replay(workflowRun);
        refreshPage();
    }

    private Component createExceptionBlock(WorkflowRun workflowRun) {
        final var span = new Span(workflowRun.getFailureMessage());
        span.addClassName("text-error");
        return span;
    }

}
