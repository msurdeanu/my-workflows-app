package org.myworkflows.domain.filter;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.User;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Getter
@Setter
@Accessors(fluent = true)
public final class UserFilter implements Filter<User> {

    private String nameCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<User> getFilterPredicate() {
        return isNotEmpty(nameCriteria)
            ? item -> containsIgnoreCase(item.getUsername(), nameCriteria)
            : _ -> true;
    }

}
