package org.myworkflows.util;

import org.junit.jupiter.api.Test;
import org.myworkflows.exception.WorkflowDuplicateException;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Mihai Surdeanu
 * @since 2.2
 */
public final class DuplicateNameUtilTest {

    @Test
    public void whenCopyNameIsTakenThenTheNextNumberIsSuggested() {
        // given
        final var taken = Set.of("Backup", "Backup (copy)", "Backup (copy 2)");

        // when and then
        assertEquals("Report (copy)", DuplicateNameUtil.suggest("Report", taken::contains));
        assertEquals("Backup (copy 3)", DuplicateNameUtil.suggest("Backup", taken::contains));
        // a copy of a copy is numbered, instead of getting a second suffix
        assertEquals("Backup (copy 3)", DuplicateNameUtil.suggest("Backup (copy)", taken::contains));
        assertEquals("Backup (copy 3)", DuplicateNameUtil.suggest("Backup (copy 2)", taken::contains));
        // a name which only looks like a copy keeps what it ends with
        assertEquals("Backup (copy) old (copy)", DuplicateNameUtil.suggest("Backup (copy) old", taken::contains));
    }

    @Test
    public void whenNameIsBlankOrTakenThenItIsRefused() {
        // given
        final var taken = Set.of("Backup");

        // when and then
        assertEquals("Backup v2", DuplicateNameUtil.requireFree("  Backup v2 ", taken::contains));
        assertEquals(WorkflowDuplicateException.Reason.NAME_MISSING,
            assertThrows(WorkflowDuplicateException.class, () -> DuplicateNameUtil.requireFree(" ", taken::contains)).getReason());
        assertEquals(WorkflowDuplicateException.Reason.NAME_MISSING,
            assertThrows(WorkflowDuplicateException.class, () -> DuplicateNameUtil.requireFree(null, taken::contains)).getReason());
        // the blanks around a name do not make it another one
        final var exception = assertThrows(WorkflowDuplicateException.class, () -> DuplicateNameUtil.requireFree(" Backup ", taken::contains));
        assertEquals(WorkflowDuplicateException.Reason.NAME_TAKEN, exception.getReason());
        assertEquals("Backup", exception.getName());
    }

}
