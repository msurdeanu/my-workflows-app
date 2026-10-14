package org.myworkflows.util;

import org.junit.jupiter.api.Test;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class PlaceholderUtilTest {

    @Test
    public void whenTextNamesPlaceholdersThenEachIsFoundOnceInTheOrderItIsFirstNamed() {
        // when
        final var names = PlaceholderUtil.findPlaceholderNames("$$(API.URL)/sales?key=$$(API_KEY_2) then $$(API.URL) again");

        // then
        assertEquals(List.of("API.URL", "API_KEY_2"), List.copyOf(names));
    }

    @Test
    public void whenTextNamesNoPlaceholderThenNoneIsFound() {
        // what is not written the way a placeholder is - lower case, one dollar, no closing bracket - is none
        assertTrue(PlaceholderUtil.findPlaceholderNames("$$(lower) $(ONE) $$(OPEN $$()").isEmpty());
        assertTrue(PlaceholderUtil.findPlaceholderNames("").isEmpty());
        assertTrue(PlaceholderUtil.findPlaceholderNames(null).isEmpty());
    }

    @Test
    public void whenPlaceholdersAreResolvedThenTheOnesWhichWereFoundAreTheOnesWhichAreAskedFor() {
        // given
        final var text = "$$(HOST):$$(PORT)";
        assertEquals(List.of("HOST", "PORT"), List.copyOf(PlaceholderUtil.findPlaceholderNames(text)));

        // when and then
        assertEquals("localhost:8080", PlaceholderUtil.resolvePlaceholders(text, Map.of("HOST", "localhost", "PORT", "8080")));
        final var exception = assertThrows(WorkflowRuntimeException.class, () -> PlaceholderUtil.resolvePlaceholders(text, Map.of("HOST", "localhost")));
        assertEquals("No placeholder found for name 'PORT'", exception.getMessage());
    }

}
