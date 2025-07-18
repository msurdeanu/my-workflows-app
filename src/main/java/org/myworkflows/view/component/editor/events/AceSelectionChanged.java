package org.myworkflows.view.component.editor.events;

import com.vaadin.flow.component.ComponentEvent;
import com.vaadin.flow.component.DomEvent;
import com.vaadin.flow.component.EventData;
import elemental.json.JsonObject;
import org.myworkflows.view.component.editor.AceEditor;
import org.myworkflows.view.component.editor.AceCursorPosition;
import org.myworkflows.view.component.editor.AceSelection;

@DomEvent("editor-selection")
public class AceSelectionChanged extends ComponentEvent<AceEditor> {

    private AceSelection selection;
    private AceCursorPosition cursorPosition;

    public AceSelectionChanged(
        AceEditor source,
        boolean fromClient,
        @EventData("event.detail.selection") JsonObject selectionObject,
        @EventData("event.detail.cursorPosition") JsonObject cursorObject) {
        super(source, fromClient);
        this.selection = new AceSelection(selectionObject);
        this.cursorPosition = new AceCursorPosition(cursorObject);
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
