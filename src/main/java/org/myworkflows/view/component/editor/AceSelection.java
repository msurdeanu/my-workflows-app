package org.myworkflows.view.component.editor;

import elemental.json.JsonObject;
import lombok.Getter;

@Getter
public class AceSelection {

    private final int startRow;
    private final int startColumn;
    private final int endRow;
    private final int endColumn;
    private final int startIndex;
    private final int endIndex;
    private final String selectedText;

    public AceSelection() {
        this.startRow = 0;
        this.startColumn = 0;
        this.endRow = 0;
        this.endColumn = 0;
        this.startIndex = 0;
        this.endIndex = 0;
        this.selectedText = "";
    }

    public AceSelection(JsonObject selectionObject) {
        JsonObject startObject = selectionObject.getObject("start");
        this.startRow = (int) startObject.getNumber("row");
        this.startColumn = (int) startObject.getNumber("column");

        JsonObject endObject = selectionObject.getObject("end");
        this.endRow = (int) endObject.getNumber("row");
        this.endColumn = (int) endObject.getNumber("column");

        JsonObject indexObject = selectionObject.getObject("index");
        this.startIndex = (int) indexObject.getNumber("start");
        this.endIndex = (int) indexObject.getNumber("end");

        this.selectedText = selectionObject.getString("selectedText");
    }

    @SuppressWarnings("BooleanExpressionComplexity")
    public boolean compareTo(AceSelection aceSelection) {
        return aceSelection.getStartRow() == this.startRow
            && aceSelection.getStartColumn() == this.startColumn
            && aceSelection.getStartIndex() == this.startIndex
            && aceSelection.getEndRow() == this.endRow
            && aceSelection.getEndColumn() == this.endColumn
            && aceSelection.getEndIndex() == this.endIndex
            && aceSelection.getSelectedText().equals(this.selectedText);
    }
}
