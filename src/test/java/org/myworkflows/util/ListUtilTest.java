package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.myworkflows.util.ListUtil.getValueAtIndex;
import static org.myworkflows.util.ListUtil.substract;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ListUtilTest {

    @Test
    public void getValueAtIndexWorksAsExpected() {
        // given
        final var values = List.of("a", "b", "c");
        final var defaultValue = "default";

        // when and then
        assertEquals(defaultValue, getValueAtIndex(values, -1, defaultValue));
        assertEquals("a", getValueAtIndex(values, 0, defaultValue));
        assertEquals(defaultValue, getValueAtIndex(values, 3, defaultValue));
    }

    @Test
    public void substractWorksAsExpected() {
        // given
        final var firstList = List.of("a", "b", "c");
        final var secondList = List.of("a", "d");

        // when and then
        final var substractedList = substract(firstList, secondList);
        assertEquals(2, substractedList.size());
        assertEquals("b", substractedList.get(0));
        assertEquals("c", substractedList.get(1));
    }

}
