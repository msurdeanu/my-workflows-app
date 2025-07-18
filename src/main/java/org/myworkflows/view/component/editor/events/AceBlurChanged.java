package org.myworkflows.view.component.editor.events;

import com.vaadin.flow.component.ComponentEvent;
import com.vaadin.flow.component.DomEvent;
import com.vaadin.flow.component.EventData;
import elemental.json.JsonObject;
import org.myworkflows.view.component.editor.AceEditor;
import org.myworkflows.view.component.editor.AceCursorPosition;
import org.myworkflows.view.component.editor.AceSelection;

@DomEvent("editor-blur")
public class AceBlurChanged extends ComponentEvent<AceEditor> {

    private String value;
    private AceSelection selection;
    private AceCursorPosition cursorPosition;

    public AceBlurChanged(
        AceEditor source,
        boolean fromClient,
        @EventData("event.detail.value") String value,
        @EventData("event.detail.selection") JsonObject selectionObject,
        @EventData("event.detail.cursorPosition") JsonObject cursorObject) {
        super(source, fromClient);
        this.value = value;
        this.selection = new AceSelection(selectionObject);
        this.cursorPosition = new AceCursorPosition(cursorObject);
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
