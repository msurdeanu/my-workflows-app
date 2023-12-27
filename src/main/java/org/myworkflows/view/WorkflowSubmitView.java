package org.myworkflows.view;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import de.f0rce.ace.enums.AceTheme;
import jakarta.annotation.security.PermitAll;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.domain.event.WorkflowSubmitEvent;
import org.myworkflows.layout.BaseLayout;
import org.myworkflows.layout.ResponsiveLayout;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@PermitAll
@Route(value = WorkflowSubmitView.ROUTE, layout = BaseLayout.class)
public class WorkflowSubmitView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "workflows/submit";

    public WorkflowSubmitView(final ApplicationManager applicationManager) {
        super();

        final var editor = new AceEditor();
        editor.setWidthFull();
        editor.setTheme(AceTheme.terminal);
        editor.setMode(AceMode.json);
        final var submitButton = new Button(getTranslation("workflow-submit.submit.button"));
        submitButton.addClickListener(event -> applicationManager.getEventBroadcaster().broadcast(WorkflowSubmitEvent.builder()
                .workflowAsString(editor.getValue())
                .build()));

        add(createHeader(getTranslation("workflow-submit.page.subtitle")));
        add(createContent(editor, submitButton));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-submit"));
    }

}
