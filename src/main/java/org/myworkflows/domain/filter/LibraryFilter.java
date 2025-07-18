package org.myworkflows.domain.filter;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.Library;

import java.util.function.Predicate;

import static org.apache.commons.lang3.StringUtils.containsIgnoreCase;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Setter
@Accessors(fluent = true)
public final class LibraryFilter implements Filter<Library> {

    private String filePathCriteria = StringUtils.EMPTY;

    @Override
    public Predicate<Library> getFilterPredicate() {
        return isNotEmpty(filePathCriteria)
            ? item -> containsIgnoreCase(item.filePath(), filePathCriteria)
            : item -> true;
    }

}
