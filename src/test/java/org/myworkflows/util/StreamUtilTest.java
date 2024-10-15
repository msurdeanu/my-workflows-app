package org.myworkflows.util;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.myworkflows.util.StreamUtil.substract;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class StreamUtilTest {

    @Test
    public void substractionWorksAsExpected() {
        // given
        final var firstList = List.of("a", "b", "c");
        final var secondList = List.of("a", "d");

        // when & then
        final var substractedList = substract(firstList, secondList);
        assertEquals(2, substractedList.size());
        assertEquals("b", substractedList.get(0));
        assertEquals("c", substractedList.get(1));
    }

}
