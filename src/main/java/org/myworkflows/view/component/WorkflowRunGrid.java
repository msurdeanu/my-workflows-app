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
import org.myworkflows.util.SecurityUtil;
import org.myworkflows.view.WorkflowRunView;
import org.myworkflows.view.WorkflowTemplateView;
import org.myworkflows.view.component.html.StandardPaginatedGrid;
import org.myworkflows.view.util.ClipboardUtil;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@RequiredArgsConstructor
public final class WorkflowRunGrid extends Composite<VerticalLayout> {

    private static final int SHORT_ID_LENGTH = 8;
    private static final String STATUS_PENDING = "workflow-runs.grid.status.pending";
    private static final String STATUS_APPROVAL = "workflow-runs.grid.status.approval";
    private static final String STATUS_PAUSED = "workflow-runs.grid.status.paused";

    private final StandardPaginatedGrid<WorkflowRun, ?> paginatedGrid = new StandardPaginatedGrid<>();

    /**
     * The runs in progress this grid was told about, each with what its row was drawn as: running, waiting for an
     * approval or paused.
     */
    private final Map<UUID, String> followedRuns = new HashMap<>();

    private final Integer currentUserId = SecurityUtil.findCurrentUserId().orElse(null);

    private final WorkflowRunService workflowRunService;

    public void refreshPage() {
        paginatedGrid.refreshPaginator();
    }

    public void setDataProvider(DataProvider<WorkflowRun, ?> dataProvider) {
        paginatedGrid.setDataProvider(dataProvider);
    }

    /**
     * Follows a run that changed without this page asking for it. A run that starts - or starts again, when it is
     * replayed - adds a row or turns one back to running, so the page on display is read again. A completed run
     * redraws its own row and nothing else, and so does a run which starts or stops to wait - for an approval, or to
     * be told how to go on while it is debugged. A command that passed changes nothing a row shows.
     */
    public void onRunChange(WorkflowRun workflowRun, boolean completed) {
        if (completed) {
            followedRuns.remove(workflowRun.getId());
            paginatedGrid.getDataProvider().refreshItem(workflowRun);
            return;
        }

        final var statusKey = findStatusKeyOfRunInProgress(workflowRun);
        final var drawnStatusKey = followedRuns.put(workflowRun.getId(), statusKey);
        if (drawnStatusKey == null) {
            paginatedGrid.refreshCurrentPage();
        } else if (!drawnStatusKey.equals(statusKey)) {
            paginatedGrid.getDataProvider().refreshItem(workflowRun);
        }
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        final var narrowColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderPhoneCell))
            .setHeader(getTranslation("workflow-runs.grid.run.column"));
        final var idColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderId))
            .setHeader(getTranslation("workflow-runs.grid.id.column"))
            .setAutoWidth(true);
        final var userColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderUser))
            .setHeader(getTranslation("workflow-runs.grid.user.column"))
            .setAutoWidth(true);
        // a template name can be arbitrarily long, so it gets a minimum and the extra space instead of its content width
        final var templateColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderTemplateId))
            .setHeader(getTranslation("workflow-runs.grid.template.column"))
            .setWidth("150px")
            .setFlexGrow(2);
        final var statusColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderStatus))
            .setHeader(getTranslation("workflow-runs.grid.status.column"))
            .setAutoWidth(true);
        final var detailsColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderDetails))
            .setHeader(getTranslation("workflow-runs.grid.details.column"))
            .setAutoWidth(true);
        final var actionsColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderActions))
            .setHeader(getTranslation("workflow-runs.grid.actions.column"))
            .setAutoWidth(true);
        paginatedGrid.setNarrowColumn(narrowColumn, List.of(idColumn, userColumn, templateColumn, statusColumn, detailsColumn, actionsColumn));

        layout.add(paginatedGrid);
        return layout;
    }

    /**
     * A phone shows the whole run in one cell: the id, then the user, the template and the status, then the run
     * time and the actions.
     */
    private Component renderPhoneCell(WorkflowRun workflowRun) {
        final var badges = new HorizontalLayout(renderUser(workflowRun), renderTemplateId(workflowRun), renderStatus(workflowRun));
        badges.addClassName("cell-badges");
        badges.setSpacing(true);
        final var actions = new HorizontalLayout(renderDetails(workflowRun), renderActions(workflowRun));
        actions.addClassName("cell-badges");
        actions.setAlignItems(FlexComponent.Alignment.CENTER);
        final var layout = new VerticalLayout(renderId(workflowRun), badges, actions);
        layout.addClassName("phone-cell");
        layout.setPadding(false);
        layout.setSpacing(false);
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

    private Component renderUser(WorkflowRun workflowRun) {
        final var badge = new Badge(workflowRunService.findUsername(workflowRun).orElseGet(() -> getTranslation(noUserLabelKey(workflowRun))));
        badge.addThemeVariants(BadgeVariant.CONTRAST, BadgeVariant.SMALL);
        return badge;
    }

    private String noUserLabelKey(WorkflowRun workflowRun) {
        if (workflowRun.getUserId() != null) {
            // the user who started the run was deleted since
            return "workflow-runs.grid.user.deleted";
        }
        // a run of a template that belongs to no user was started by the cron of that template
        return workflowRun.getWorkflowTemplateId() != null
            ? "workflow-runs.grid.user.scheduler"
            : "workflow-runs.grid.user.unknown";
    }

    /**
     * @return the translation which says what a run in progress is doing. A run which waits for somebody - to approve
     *     it, or to tell it how to go on - stands out from the ones which only need time.
     */
    private String findStatusKeyOfRunInProgress(WorkflowRun workflowRun) {
        if (workflowRun.isWaitingForApproval()) {
            return STATUS_APPROVAL;
        }
        return workflowRun.isPaused() ? STATUS_PAUSED : STATUS_PENDING;
    }

    private Component renderStatus(WorkflowRun workflowRun) {
        if (workflowRun.isRunning()) {
            final var statusKey = findStatusKeyOfRunInProgress(workflowRun);
            final var badge = new Badge(getTranslation(statusKey));
            badge.addThemeVariants(STATUS_PENDING.equals(statusKey) ? BadgeVariant.CONTRAST : BadgeVariant.WARNING, BadgeVariant.SMALL);
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
        button.addClickListener(_ -> new WorkflowRunDetailsDialog(workflowRun, workflowRunService.countReports(workflowRun)).open());
        return button;
    }

    private Component renderActions(WorkflowRun workflowRun) {
        final var layout = new HorizontalLayout();
        layout.addClassName("cell-badges");
        if (workflowRun.isRunning()) {
            if (workflowRun.isWaitingForApproval()) {
                final var approvalButton = new Button(VaadinIcon.USER_CHECK.create());
                approvalButton.addThemeVariants(ButtonVariant.SMALL, ButtonVariant.PRIMARY);
                approvalButton.setTooltipText(getTranslation("workflow-runs.grid.approval.tooltip"));
                approvalButton.setAriaLabel(getTranslation("workflow-runs.grid.approval.tooltip"));
                approvalButton.addClickListener(_ -> openApproval(workflowRun));
                layout.add(approvalButton);
            }
            final var cancelButton = new Button(VaadinIcon.CLOSE.create());
            cancelButton.addThemeVariants(ButtonVariant.SMALL);
            cancelButton.addClickListener(_ -> cancel(workflowRun));
            layout.add(cancelButton);
        } else {
            final var deleteButton = new Button(VaadinIcon.TRASH.create());
            deleteButton.addThemeVariants(ButtonVariant.SMALL);
            deleteButton.addClickListener(_ -> delete(workflowRun));
            layout.add(deleteButton);
            if (workflowRun.isReplayableBy(currentUserId)) {
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

    private void openApproval(WorkflowRun workflowRun) {
        // the approval is the one the run waits for now, which the row may not have been drawn for
        ofNullable(workflowRun.getApproval()).ifPresentOrElse(approval -> new WorkflowApprovalDialog(workflowRun, approval, workflowRunService).open(),
            () -> {
                Notification.show(getTranslation("dialog.approval.gone.message"));
                paginatedGrid.getDataProvider().refreshItem(workflowRun);
            });
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
