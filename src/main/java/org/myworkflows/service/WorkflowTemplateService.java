package org.myworkflows.service;

import com.vaadin.flow.data.provider.Query;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.WorkflowTemplateFilter;
import org.myworkflows.domain.event.WorkflowTemplateOnUpdateEvent;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
@RequiredArgsConstructor
public final class WorkflowTemplateService {

    private static final Predicate<WorkflowTemplate> ALWAYS_TRUE_PREDICATE = testScenario -> true;

    private static final Map<Integer, WorkflowTemplate> ALL_WORKFLOWS = new HashMap<>();

    private final Lock lock = new ReentrantLock();

    private final ApplicationManager applicationManager;

    @Order(10)
    @EventListener(ApplicationReadyEvent.class)
    public void loadAll() {
        applicationManager.getBeanOfType(WorkflowTemplateRepository.class)
                .findAll()
                .forEach(this::loadAndSchedule);
    }

    public void loadAndSchedule(@NonNull final WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            ofNullable(ALL_WORKFLOWS.put(workflowTemplate.getId(), workflowTemplate))
                    .filter(WorkflowTemplate::isEnabledForScheduling)
                    .ifPresent(oldItem -> applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                            .unschedule(oldItem.getDefinition()));
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                        .schedule(workflowTemplate.getDefinition(), workflowTemplate.getCron());
            }
        } finally {
            lock.unlock();
        }
    }

    public boolean changeDefinition(@NonNull final WorkflowTemplate workflowTemplate, final String newDefinition) {
        var isOperationPerformed = false;

        lock.lock();
        try {
            workflowTemplate.setDefinition(fromJsonToObject(newDefinition, WorkflowDefinition.class));
            isOperationPerformed = true;
        } finally {
            lock.unlock();
        }

        if (isOperationPerformed) {
            applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
        }
        return isOperationPerformed;
    }

    public Stream<WorkflowTemplate> findBy(final Query<WorkflowTemplate, WorkflowTemplateFilter> query) {
        return query.getFilter()
                .map(filter -> getAll(filter, query.getOffset(), query.getLimit()))
                .orElseGet(this::getAll);
    }

    public int countBy(final Query<WorkflowTemplate, WorkflowTemplateFilter> query) {
        return query.getFilter()
                .map(this::getAllSize)
                .orElseGet(this::getAllSize)
                .intValue();
    }

    public Stream<WorkflowTemplate> getAll() {
        return getAll(new WorkflowTemplateFilter(), 0, Long.MAX_VALUE);
    }

    public Stream<WorkflowTemplate> getAll(final WorkflowTemplateFilter filter, final long offset, final long limit) {
        return ALL_WORKFLOWS.values().stream()
                .filter(getPredicateByNameCriteria(filter.getByNameCriteria()))
                .skip(offset)
                .limit(limit);
    }

    public long getAllSize() {
        return getAll().count();
    }

    public long getAllSize(final WorkflowTemplateFilter filter) {
        return getAll(filter, 0, Long.MAX_VALUE).count();
    }

    private Predicate<WorkflowTemplate> getPredicateByNameCriteria(final String byNameCriteria) {
        return StringUtils.isNotEmpty(byNameCriteria)
                ? workflowTemplate -> StringUtils.containsIgnoreCase(workflowTemplate.getName(), byNameCriteria)
                : ALWAYS_TRUE_PREDICATE;
    }

}