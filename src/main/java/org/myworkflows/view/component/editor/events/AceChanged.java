package org.myworkflows.view.component.editor.events;

import com.vaadin.flow.component.ComponentEvent;
import com.vaadin.flow.component.DomEvent;
import com.vaadin.flow.component.EventData;
import lombok.Getter;
import org.myworkflows.view.component.editor.AceEditor;

@DomEvent("editor-change")
public class AceChanged extends ComponentEvent<AceEditor> {

    @Getter
    private String value;

    public AceChanged(AceEditor source, boolean fromClient, @EventData("event.detail.value") String value) {
        super(source, fromClient);
        this.value = value;
    }

}
