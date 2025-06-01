package org.myworkflows.domain.filter;

import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.Library;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.1.0
 */
@Getter
@Setter
public final class LibraryFilter implements Filter<Library> {

    private String byFileNameCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<Library> getFilterPredicate() {
        return isNotEmpty(byFileNameCriteria)
            ? item -> containsIgnoreCase(item.filePath(), byFileNameCriteria)
            : item -> true;
    }

}
