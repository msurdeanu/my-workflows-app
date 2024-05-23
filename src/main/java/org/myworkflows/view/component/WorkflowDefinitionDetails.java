package org.myworkflows.view.component;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import de.f0rce.ace.AceEditor;
import de.f0rce.ace.enums.AceMode;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionEventHandler;
import org.myworkflows.serializer.JsonFactory;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@RequiredArgsConstructor
public final class WorkflowDefinitionDetails extends Composite<VerticalLayout> {

    private final AceEditor editor = new AceEditor();

    private final Button saveButton = new Button(getTranslation("workflow-definitions.main-grid.button.save"));

    private final WorkflowDefinitionEventHandler workflowDefinitionEventHandler;

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setDefaultHorizontalComponentAlignment(FlexComponent.Alignment.CENTER);

        editor.addClassName("ace-editor");
        editor.setMode(AceMode.json);
        editor.setAutoComplete(true);
        editor.setLiveAutocompletion(true);
        layout.add(editor);

        saveButton.setEnabled(false);
        saveButton.setWidthFull();
        saveButton.addThemeVariants(ButtonVariant.LUMO_SMALL);
        layout.add(saveButton);

        return layout;
    }

    public void setDetails(WorkflowDefinition workflowDefinition) {
        editor.setValue(JsonFactory.toPrettyString(workflowDefinition.getScript(), ""));
        editor.addAceChangedListener(event -> saveButton.setEnabled(isNewDefinition(workflowDefinition, event.getValue())));

        saveButton.addClickListener(event -> workflowDefinitionEventHandler.onScriptUpdated(workflowDefinition.getId(), editor.getValue()));
    }

    private boolean isNewDefinition(WorkflowDefinition workflowDefinition, String newScript) {
        return !JsonFactory.toPrettyString(workflowDefinition.getScript(), "").equals(newScript);
    }

}
