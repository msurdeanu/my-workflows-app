package org.myworkflows.view;

import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.auth.AnonymousAllowed;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import de.f0rce.ace.enums.AceTheme;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.layout.BaseLayout;
import org.myworkflows.layout.ResponsiveLayout;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@AnonymousAllowed
@Route(value = WorkflowSubmitView.ROUTE, layout = BaseLayout.class)
public class WorkflowSubmitView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "workflows/submit";

    public WorkflowSubmitView() {
        super();

        final var editor = new AceEditor();
        editor.setWidthFull();
        editor.setTheme(AceTheme.terminal);
        editor.setMode(AceMode.json);

        add(createHeader(getTranslation("workflow-submit.page.subtitle")));
        add(createContent(editor));
        add(createFooter());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.workflow-submit"));
    }

}
