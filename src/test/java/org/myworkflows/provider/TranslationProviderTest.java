package org.myworkflows.provider;

import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class TranslationProviderTest {

    @Test
    public void whenKeyIsNullThenTranslationIsNull() {
        // given
        final var provider = new TranslationProvider();

        // when & then
        assertNull(provider.getTranslation(null, Locale.ENGLISH));
    }

    @Test
    public void whenKeyIsPrettyTimeFormatTheTranslationIsSetToRelativeTime() {
        // given
        final var provider = new TranslationProvider();

        // when & then
        assertEquals("6 decades ago", provider.getTranslation("pretty.time.format", Locale.ENGLISH, Instant.ofEpochSecond(0)));
    }

    @Test
    public void whenKeyIsNotFoundThenTranslationReturnsTheKey() {
        // given
        final var provider = new TranslationProvider();
        final var notFoundKey = "bla.bla.bla";

        // when & then
        assertEquals(notFoundKey, provider.getTranslation(notFoundKey, Locale.ENGLISH));
    }

    @Test
    public void whenKeyIsFoundThenTranslationIsFormattedCorrectly() {
        // given
        final var provider = new TranslationProvider();
        // when & then
        assertEquals("My Workflows - Test", provider.getTranslation("site.base.title", Locale.ENGLISH, "Test"));
    }

}
