package org.myworkflows.view.component;

import com.vaadin.flow.component.ComponentEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.KeyModifier;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.shared.Registration;
import de.f0rce.ace.AceEditor;
import lombok.Getter;
import org.myworkflows.view.util.ScriptBlock;

import static java.util.Optional.ofNullable;

/**
 * Edits a java.script or groovy.script value in a dedicated editor. The dialog does not change the workflow definition
 * by itself: it fires a {@link SaveEvent} with the new script and the listener decides where to write it.
 *
 * @author Mihai Surdeanu
 * @since 1.3
 */
public final class ScriptEditorDialog extends Dialog {

    private final AceEditor scriptEditor = new AceEditor();
    private final String initialScript;

    public ScriptEditorDialog(ScriptBlock scriptBlock, boolean readOnly) {
        initialScript = scriptBlock.script();

        addClassName("script-editor-dialog");
        setHeaderTitle(ofNullable(scriptBlock.commandName())
            .map(commandName -> getTranslation("dialog.script-editor.header", commandName, scriptBlock.language().getInputName()))
            .orElseGet(() -> scriptBlock.language().getInputName()));
        setWidth("80vw");
        setHeight("80vh");
        setDraggable(true);
        setResizable(true);
        // an accidental click or escape key should not throw away the changes
        setCloseOnEsc(false);
        setCloseOnOutsideClick(false);

        scriptEditor.setMode(scriptBlock.language().getMode());
        scriptEditor.setSofttabs(true);
        scriptEditor.setTabSize(2);
        scriptEditor.setAutoComplete(true);
        scriptEditor.setLiveAutocompletion(true);
        scriptEditor.setEnableSnippets(true);
        scriptEditor.setReadOnly(readOnly);
        scriptEditor.setValue(initialScript);
        scriptEditor.setCursorPosition(scriptBlock.line(), 0, true);
        scriptEditor.setSizeFull();
        add(scriptEditor);

        final var cancelButton = new Button(getTranslation("dialog.script-editor.button.cancel"), _ -> close());
        cancelButton.addThemeVariants(ButtonVariant.TERTIARY);
        final var saveButton = new Button(getTranslation("dialog.script-editor.button.save"), _ -> scriptEditor.runAfterSync(this::save));
        saveButton.addThemeVariants(ButtonVariant.PRIMARY);
        saveButton.setTooltipText(getTranslation("dialog.script-editor.button.save.tooltip"));
        saveButton.addClickShortcut(Key.KEY_S, KeyModifier.CONTROL).listenOn(this);
        saveButton.setVisible(!readOnly);
        getFooter().add(cancelButton, saveButton);
    }

    public Registration addSaveListener(ComponentEventListener<SaveEvent> listener) {
        return addListener(SaveEvent.class, listener);
    }

    private void save() {
        // the guard protects against a save triggered twice (e.g. a double click) before the dialog was closed
        if (!isOpened()) {
            return;
        }

        close();
        final var script = scriptEditor.getValue();
        if (!script.equals(initialScript)) {
            fireEvent(new SaveEvent(this, script));
        }
    }

    /**
     * @author Mihai Surdeanu
     * @since 1.3
     */
    @Getter
    public static final class SaveEvent extends ComponentEvent<ScriptEditorDialog> {

        private final String script;

        public SaveEvent(ScriptEditorDialog source, String script) {
            super(source, false);
            this.script = script;
        }

    }

}
