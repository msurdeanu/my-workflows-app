package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.ExecutionContext;
import org.myworkflows.domain.ExecutionContextFilter;
import org.springframework.stereotype.Service;

import java.util.function.Predicate;
import java.util.stream.Stream;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public class WorkflowRunService {

    private static final Predicate<ExecutionContext> ALWAYS_TRUE_PREDICATE = item -> true;

    private final WorkflowRunCacheService workflowRunCacheService;

    public Stream<ExecutionContext> findBy(Query<ExecutionContext, ExecutionContextFilter> query) {
        return query.getFilter()
            .map(filter -> getAll(filter, query.getOffset(), query.getLimit()))
            .orElseGet(this::getAll);
    }

    public int countBy(Query<ExecutionContext, ExecutionContextFilter> query) {
        return query.getFilter()
            .map(this::getAllSize)
            .orElseGet(this::getAllSize)
            .intValue();
    }

    public Stream<ExecutionContext> getAll() {
        return getAll(new ExecutionContextFilter(), 0, Long.MAX_VALUE);
    }

    public Stream<ExecutionContext> getAll(ExecutionContextFilter filter, long offset, long limit) {
        return workflowRunCacheService.getAllValuesAsStream()
            .filter(getPredicateByWorkflowIdCriteria(filter.getByWorkflowIdCriteria()))
            .skip(offset)
            .limit(limit);
    }

    public long getAllSize() {
        return getAll().count();
    }

    public long getAllSize(ExecutionContextFilter executionContextFilter) {
        return getAll().count();
    }

    private Predicate<ExecutionContext> getPredicateByWorkflowIdCriteria(String byWorkflowIdCriteria) {
        return isNotEmpty(byWorkflowIdCriteria)
            ? executionContext -> StringUtils.containsIgnoreCase(executionContext.getWorkflowId().toString(), byWorkflowIdCriteria)
            : ALWAYS_TRUE_PREDICATE;
    }

}
