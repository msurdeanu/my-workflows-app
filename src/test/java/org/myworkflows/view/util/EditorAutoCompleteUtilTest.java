package org.myworkflows.view.util;

import org.junit.jupiter.api.Test;
import org.myworkflows.view.component.editor.AceEditor;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class EditorAutoCompleteUtilTest {

    @Test
    public void testAutoComplete() {
        // given
        AceEditor aceEditor = new AceEditor();
        aceEditor.setAutoComplete(true);

        // when and then
        EditorAutoCompleteUtil.apply(aceEditor);

        final var staticWordCompleterList = aceEditor.getStaticWordCompleter();
        assertEquals(2, staticWordCompleterList.size());
        final var dynamicWordCompleterList = aceEditor.getDynamicWordCompleter();
        assertEquals(1, dynamicWordCompleterList.size());
    }

}
