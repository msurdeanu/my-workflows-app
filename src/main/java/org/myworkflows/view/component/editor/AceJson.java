package org.myworkflows.view.component.editor;

public class AceJson {

    public static String generateSelectionJson(int startRow, int startColumn, int endRow, int endColumn) {
        return "{\"start\":{\"row\":" + startRow + ",\"column\":" + startColumn + "},\"end\":{\"row\": " + endRow + ", \"column\": " + endColumn + " }}";
    }

    public static String generateSelectionJson(AceSelection selection) {
        return "{\"start\": { \"row\": " + selection.getStartRow() + ", \"column\": "
            + selection.getStartColumn() + " }, " + "\"end\": { \"row\": " + selection.getEndRow()
            + ", \"column\": " + selection.getEndColumn() + " }" + "}";
    }

    public static String generateCursorPositionJson(int row, int column) {
        return "{\"row\": " + (row + 1) + ", \"column\": " + column + "}";
    }

    public static String generateCursorPositionJson(AceCursorPosition cursorPosition) {
        return "{\"row\": " + (cursorPosition.getRow() + 1) + ", \"column\": " + cursorPosition.getColumn() + "}";
    }

}
