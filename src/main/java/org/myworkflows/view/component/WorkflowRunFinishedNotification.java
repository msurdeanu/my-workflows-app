package org.myworkflows.view.component;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.router.RouterLink;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.view.WorkflowRunView;

/**
 * Tells the user who started a run, wherever they are in the application, that it is over and how it ended.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class WorkflowRunFinishedNotification extends Notification {

    static final int DURATION = 10_000;

    private static final int SHORT_ID_LENGTH = 8;

    /**
     * @param templateName the name of the template the run came from, or {@code null} for a run of no template.
     */
    public WorkflowRunFinishedNotification(WorkflowRun workflowRun, String templateName) {
        final var runId = workflowRun.getId().toString();
        final var text = templateName == null
            ? getTranslation(messageKeyOf(workflowRun, false), shortIdOf(workflowRun), workflowRun.getHumanReadableDuration())
            : getTranslation(messageKeyOf(workflowRun, true), shortIdOf(workflowRun), templateName, workflowRun.getHumanReadableDuration());
        final var routerLink = new RouterLink(getTranslation("workflow-runs.notification.finished.open"), WorkflowRunView.class, runId);
        routerLink.getElement().addEventListener("click", _ -> close());
        final var content = new Div(new Span(text), routerLink);
        content.addClassName("workflow-run-notification");
        add(content);
        addThemeVariants(isFailed(workflowRun) ? NotificationVariant.ERROR : NotificationVariant.SUCCESS);
        setDuration(DURATION);
        setPosition(Position.BOTTOM_START);
    }

    /**
     * @param userId the user of the session which is asked, or {@code null} for a session of nobody.
     * @param completed whether the run is over, as the change of the run says.
     * @return whether that user is to be told about that change: a run they started has just ended. A run of the cron
     *     of a template belongs to no user, so nobody is told about it.
     */
    public static boolean isFor(Integer userId, WorkflowRun workflowRun, boolean completed) {
        return completed && userId != null && userId.equals(workflowRun.getUserId());
    }

    static String messageKeyOf(WorkflowRun workflowRun, boolean ofTemplate) {
        return "workflow-runs.notification.finished." + (isFailed(workflowRun) ? "error" : "success") + (ofTemplate ? ".template" : "");
    }

    static String shortIdOf(WorkflowRun workflowRun) {
        return "#" + workflowRun.getId().toString().substring(0, SHORT_ID_LENGTH);
    }

    private static boolean isFailed(WorkflowRun workflowRun) {
        return workflowRun.getFailureMessage() != null;
    }

}
