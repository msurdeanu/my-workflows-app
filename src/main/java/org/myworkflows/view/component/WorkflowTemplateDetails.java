package org.myworkflows.view.component;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateEventHandler;
import org.myworkflows.serializer.JsonFactory;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowTemplateDetails extends Composite<VerticalLayout> {

    private final AceEditor editor = new AceEditor();

    private final Button saveButton = new Button(getTranslation("workflow-templates.main-grid.button.save"));

    private final WorkflowTemplateEventHandler workflowTemplateEventHandler;

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setDefaultHorizontalComponentAlignment(FlexComponent.Alignment.CENTER);

        editor.addClassName("ace-editor");
        editor.setMode(AceMode.json);
        layout.add(editor);

        saveButton.setEnabled(false);
        saveButton.setWidthFull();
        saveButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        layout.add(saveButton);

        return layout;
    }

    public void setDetails(WorkflowTemplate workflowTemplate) {
        editor.setValue(JsonFactory.toPrettyString(workflowTemplate.getDefinition(), ""));
        editor.addAceChangedListener(event -> saveButton.setEnabled(isNewDefinition(workflowTemplate, event.getValue())));

        saveButton.addClickListener(event -> workflowTemplateEventHandler.onDefinitionChanged(workflowTemplate.getId(), editor.getValue()));
    }

    private boolean isNewDefinition(WorkflowTemplate workflowTemplate, String newDefinition) {
        return !JsonFactory.toPrettyString(workflowTemplate.getDefinition(), "").equals(newDefinition);
    }

}
