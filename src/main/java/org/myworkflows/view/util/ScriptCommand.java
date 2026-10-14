package org.myworkflows.view.util;

/**
 * A command of a workflow definition as the editor shows it: where it stands among the commands it is listed with, and
 * the lines it is written on. Rows are 0-based, like in the editor.
 *
 * @param finalCommand whether the command is listed under {@code finallyCommands} instead of {@code commands}.
 * @param index the position of the command in its list.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record ScriptCommand(boolean finalCommand, int index, String name, int firstRow, int lastRow) {

    public boolean contains(int row) {
        return firstRow <= row && row <= lastRow;
    }

}
