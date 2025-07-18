package org.myworkflows.view.component.editor.events;

import com.vaadin.flow.component.ComponentEvent;
import lombok.Getter;
import org.myworkflows.view.component.editor.AceEditor;

public class AceValueChanged extends ComponentEvent<AceEditor> {

    @Getter
    private String value;

    public AceValueChanged(AceEditor source, boolean fromClient, String value) {
        super(source, fromClient);
        this.value = value;
    }

}
