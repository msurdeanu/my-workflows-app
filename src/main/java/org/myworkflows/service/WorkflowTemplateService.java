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
import org.myworkflows.domain.event.WorkflowTemplateOnDeleteEvent;
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
import static org.apache.commons.lang3.StringUtils.isNotEmpty;
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

    public void changeActivation(@NonNull final Integer workflowId) {
        lock.lock();
        try {
            ofNullable(ALL_WORKFLOWS.get(workflowId)).ifPresent(workflowTemplate -> {
                workflowTemplate.toggleOnEnabling();
                if (workflowTemplate.isEnabled()) {
                    applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                        .unschedule(workflowTemplate.getDefinition());
                } else {
                    applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                        .schedule(workflowTemplate.getDefinition(), workflowTemplate.getCron());
                }
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
            });
        } finally {
            lock.unlock();
        }
    }

    public void changeCron(final Integer workflowId, final String newCron) {
        lock.lock();
        try {
            ofNullable(ALL_WORKFLOWS.get(workflowId)).ifPresent(workflowTemplate -> {
                if (workflowTemplate.isEnabled()) {
                    applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                        .unschedule(workflowTemplate.getDefinition());
                }
                workflowTemplate.setCron(newCron);
                if (workflowTemplate.isEnabled()) {
                    applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                        .schedule(workflowTemplate.getDefinition(), newCron);
                }
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
            });
        } finally {
            lock.unlock();
        }
    }

    public boolean changeDefinition(final Integer workflowId, final String newDefinition) {
        lock.lock();
        try {
            return ofNullable(ALL_WORKFLOWS.get(workflowId)).map(workflowTemplate -> {
                workflowTemplate.setDefinition(fromJsonToObject(newDefinition, WorkflowDefinition.class));
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
                return true;
            }).orElse(false);
        } catch (Exception notUsed) {
            return false;
        } finally {
            lock.unlock();
        }
    }

    public boolean changeName(final Integer workflowId, final String newName) {
        lock.lock();
        try {
            return ofNullable(ALL_WORKFLOWS.get(workflowId)).map(workflowTemplate -> {
                workflowTemplate.setName(newName);
                applicationManager.getBeanOfType(EventBroadcaster.class)
                    .broadcast(WorkflowTemplateOnUpdateEvent.builder().workflowTemplate(workflowTemplate).build());
                return true;
            }).orElse(false);
        } catch (Exception notUsed) {
            return false;
        } finally {
            lock.unlock();
        }
    }

    public void delete(final Integer workflowId) {
        lock.lock();
        try {
            final var workflowTemplate = ALL_WORKFLOWS.remove(workflowId);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                    .unschedule(workflowTemplate.getDefinition());
            }
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowTemplateOnDeleteEvent.builder().workflowTemplate(workflowTemplate).build());
        } finally {
            lock.unlock();
        }
    }

    public void scheduleNow(final Integer workflowId) {
        ofNullable(ALL_WORKFLOWS.get(workflowId)).ifPresent(workflowTemplate -> {
            applicationManager.getBeanOfType(WorkflowSchedulerService.class)
                .scheduleNowAsync(workflowTemplate.getDefinition());
        });
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
            .filter(getPredicateByIdCriteria(filter.getByIdCriteria()))
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

    private Predicate<WorkflowTemplate> getPredicateByIdCriteria(final int byIdCriteria) {
        return byIdCriteria > 0
            ? workflowTemplate -> workflowTemplate.getId() == byIdCriteria
            : ALWAYS_TRUE_PREDICATE;
    }

    private Predicate<WorkflowTemplate> getPredicateByNameCriteria(final String byNameCriteria) {
        return isNotEmpty(byNameCriteria)
            ? workflowTemplate -> StringUtils.containsIgnoreCase(workflowTemplate.getName(), byNameCriteria)
            : ALWAYS_TRUE_PREDICATE;
    }

}
