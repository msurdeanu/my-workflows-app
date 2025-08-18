package org.myworkflows.view.component.editor;

import elemental.json.JsonObject;
import lombok.Getter;

@Getter
public class AceCursorPosition {

    private final int row;
    private final int column;
    private final int index;

    public AceCursorPosition() {
        this.row = 0;
        this.column = 0;
        this.index = 0;
    }

    public AceCursorPosition(JsonObject cursorObject) {
        this.row = (int) cursorObject.getNumber("row");
        this.column = (int) cursorObject.getNumber("column");
        this.index = (int) cursorObject.getNumber("index");
    }

}