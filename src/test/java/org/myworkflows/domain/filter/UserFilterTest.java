package org.myworkflows.domain.filter;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.User;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class UserFilterTest {

    @Test
    public void whenNoCriteriaIsSetThenEveryUserMatches() {
        assertTrue(new UserFilter().getFilterPredicate().test(User.of("somebody", "a hash")));
    }

    @Test
    public void whenNameCriteriaIsSetThenUsersMatchByAPartOfTheirNameWhateverItsCase() {
        // given
        final var predicate = new UserFilter().nameCriteria("MEBO").getFilterPredicate();

        // when and then
        assertTrue(predicate.test(User.of("somebody", "a hash")));
        assertFalse(predicate.test(User.of("nobody", "a hash")));
    }

}
