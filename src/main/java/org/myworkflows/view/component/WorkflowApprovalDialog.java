package org.myworkflows.view.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunApproval;
import org.myworkflows.service.WorkflowRunService;
import org.myworkflows.util.LangUtil;

import java.time.Duration;
import java.time.Instant;

/**
 * Shows what a workflow run waits to be approved for, and takes the answer: approve or reject. The run is decided in a
 * dialog rather than by a button of its row, so that neither answer is given by a click which was not meant.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowApprovalDialog extends Dialog {

    private static final Duration SHORTEST_TIME_LEFT = Duration.ofSeconds(1);

    private final WorkflowRun workflowRun;
    private final WorkflowRunApproval approval;
    private final WorkflowRunService workflowRunService;

    public WorkflowApprovalDialog(WorkflowRun workflowRun, WorkflowRunApproval approval, WorkflowRunService workflowRunService) {
        this.workflowRun = workflowRun;
        this.approval = approval;
        this.workflowRunService = workflowRunService;

        setHeaderTitle(getTranslation("dialog.approval.header", approval.getCommandName()));
        setWidth("min(520px, 96vw)");

        final var message = new Div(StringUtils.defaultIfBlank(approval.getMessage(), getTranslation("dialog.approval.no-message.text")));
        message.addClassName("workflow-approval-message");
        final var layout = new VerticalLayout(message, new Div(getTranslation("dialog.approval.text", describeTimeLeft())));
        layout.setPadding(false);
        add(layout);

        final var closeButton = new Button(getTranslation("dialog.approval.button.close"), _ -> close());
        closeButton.addThemeVariants(ButtonVariant.TERTIARY);
        final var rejectButton = new Button(getTranslation("dialog.approval.button.reject"), VaadinIcon.CLOSE.create(), _ -> decide(false));
        rejectButton.addThemeVariants(ButtonVariant.ERROR);
        final var approveButton = new Button(getTranslation("dialog.approval.button.approve"), VaadinIcon.CHECK.create(), _ -> decide(true));
        approveButton.addThemeVariants(ButtonVariant.PRIMARY);
        getFooter().add(closeButton, rejectButton, approveButton);
    }

    private String describeTimeLeft() {
        final var timeLeft = Duration.between(Instant.now(), approval.getDeadline());
        return LangUtil.describe(timeLeft.compareTo(SHORTEST_TIME_LEFT) < 0 ? SHORTEST_TIME_LEFT : timeLeft);
    }

    private void decide(boolean approved) {
        close();
        if (workflowRunService.decideApproval(workflowRun, approval, approved)) {
            Notification.show(getTranslation(approved ? "dialog.approval.approved.message" : "dialog.approval.rejected.message"));
        } else {
            // somebody else decided in the meantime, the approval timed out or the run was cancelled
            Notification.show(getTranslation("dialog.approval.gone.message"));
        }
    }

}
