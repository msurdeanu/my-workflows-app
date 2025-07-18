package org.myworkflows.view.component.editor.events;

import com.vaadin.flow.component.ComponentEvent;
import org.myworkflows.view.component.editor.AceEditor;
import org.myworkflows.view.component.editor.AceCursorPosition;
import org.myworkflows.view.component.editor.AceSelection;

public class AceForceSyncEvent extends ComponentEvent<AceEditor> {

    private String value;
    private AceSelection selection;
    private AceCursorPosition cursorPosition;

    public AceForceSyncEvent(
        AceEditor source,
        boolean fromClient,
        String value,
        AceSelection selection,
        AceCursorPosition cursorPosition) {
        super(source, fromClient);
        this.value = value;
        this.selection = selection;
        this.cursorPosition = cursorPosition;
    }

    /**
     * Returns the current value of the editor.
     *
     * @return {@link String}
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Returns the current selection of the editor.
     *
     * @return {@link AceSelection}
     */
    public AceSelection getSelection() {
        return this.selection;
    }

    /**
     * Returns the current cursor position of the editor.
     *
     * @return {@link AceCursorPosition}
     */
    public AceCursorPosition getCursorPosition() {
        return this.cursorPosition;
    }
}
