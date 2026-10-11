package org.myworkflows.domain.filter;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowRun;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class WorkflowRunFilterTest {

    @Test
    public void whenNoCriteriaIsSetThenEveryRunIsAccepted() {
        final var predicate = new WorkflowRunFilter().getFilterPredicate();

        assertTrue(predicate.test(new WorkflowRun(null, Map.of(), 7)));
        assertTrue(predicate.test(new WorkflowRun(1, Map.of())));
    }

    @Test
    public void whenUserCriteriaIsSetThenOnlyRunsOfThatUserAreAccepted() {
        final var predicate = new WorkflowRunFilter().userIdCriteria(7).getFilterPredicate();

        assertTrue(predicate.test(new WorkflowRun(null, Map.of(), 7)));
        assertFalse(predicate.test(new WorkflowRun(null, Map.of(), 8)));
        // a run scheduled by its template belongs to no user
        assertFalse(predicate.test(new WorkflowRun(1, Map.of())));
    }

    @Test
    public void whenUserAndTemplateCriteriaAreSetThenBothHaveToMatch() {
        final var predicate = new WorkflowRunFilter().templateIdCriteria(1).userIdCriteria(7).getFilterPredicate();

        assertTrue(predicate.test(new WorkflowRun(1, Map.of(), 7)));
        assertFalse(predicate.test(new WorkflowRun(2, Map.of(), 7)));
        assertFalse(predicate.test(new WorkflowRun(1, Map.of(), 8)));
    }

}
