package org.myworkflows.domain;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRunVariable.Change;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowRunVariableTest {

    @Test
    public void whenNobodyLookedBeforeThenVariablesAreSortedByNameAndNotMarked() {
        // given
        final var cache = new WorkflowRunCache(UUID.randomUUID());
        cache.put("orders", new ArrayList<>(List.of(1041, 1042)));
        cache.put("count", 2);

        // when
        final var variables = WorkflowRunVariable.allOf(cache, null);

        // then
        assertEquals(List.of(new WorkflowRunVariable("count", "integer", "2", Change.NONE),
            new WorkflowRunVariable("orders", "arraylist", "[1041, 1042]", Change.NONE)), variables);
        assertFalse(variables.getFirst().isMarked());
    }

    @Test
    public void whenValueChangesInPlaceThenItIsMarkedAsChanged() {
        // given a list a command adds to, without putting it again
        final var cache = new WorkflowRunCache(UUID.randomUUID());
        final var orders = new ArrayList<>(List.of(1041));
        cache.put("orders", orders);
        final var previous = WorkflowRunVariable.allOf(cache, null);

        // when
        orders.add(1042);
        final var variables = WorkflowRunVariable.allOf(cache, previous);

        // then what was written down did not follow the list
        assertEquals("[1041]", previous.getFirst().value());
        assertEquals(new WorkflowRunVariable("orders", "arraylist", "[1041, 1042]", Change.CHANGED), variables.getFirst());
        assertTrue(variables.getFirst().isMarked());
    }

    @Test
    public void whenTypeChangesButNotWhatItReadsLikeThenItIsMarkedAsChanged() {
        // given
        final var cache = new WorkflowRunCache(UUID.randomUUID());
        cache.put("count", "3");
        final var previous = WorkflowRunVariable.allOf(cache, null);

        // when
        cache.put("count", 3);

        // then
        assertEquals(Change.CHANGED, WorkflowRunVariable.allOf(cache, previous).getFirst().change());
    }

    @Test
    public void whenVariableIsNamedAfterAPasswordThenItsValueIsMasked() {
        // given
        final var cache = new WorkflowRunCache(UUID.randomUUID());
        cache.put("sshExec.password", "s3cret");

        // when and then
        assertEquals("******", WorkflowRunVariable.allOf(cache, null).getFirst().value());
    }

}
