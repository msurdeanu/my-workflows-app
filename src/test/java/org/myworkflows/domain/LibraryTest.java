package org.myworkflows.domain;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.time.Instant;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class LibraryTest {

    @Test
    public void whenLibraryIsOnlyNamedThenNothingIsKnownAboutItsFile() {
        // when
        final var library = Library.of(Path.of("libs", "my-lib.jar").toString(), false);

        // then
        assertEquals("my-lib.jar", library.fileName());
        assertEquals(List.of(), library.commands());
        assertEquals(Library.UNKNOWN_SIZE, library.size());
        assertNull(library.modified());
        assertNull(library.checksum());
        assertFalse(library.hasFileFacts());
    }

    @Test
    public void whenFileOfTheLibraryWasReadThenItsFactsAreKeptNextToTheRest() {
        // given
        final var modified = Instant.parse("2026-10-12T10:00:00Z");

        // when
        final var library = Library.of("my-lib.jar", true, List.of("greet")).withFileFacts(0, modified, "abc");

        // then an empty file is still a file which was read
        assertTrue(library.hasFileFacts());
        assertEquals(new Library("my-lib.jar", true, List.of("greet"), 0, modified, "abc"), library);
    }

}
