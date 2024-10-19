package org.myworkflows.service;

import lombok.NonNull;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.InternalCacheManager.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowTemplateService extends CacheableDataService<WorkflowTemplate, WorkflowTemplateFilter> {

    public WorkflowTemplateService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_TEMPLATE);
    }

    public void loadAndSchedule(@NonNull WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            ofNullable(cache.get(workflowTemplate.getId(), WorkflowTemplate.class))
                .filter(WorkflowTemplate::isEnabledForScheduling)
                .ifPresent(oldItem -> applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate));
            cache.put(workflowTemplate.getId(), workflowTemplate);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .schedule(workflowTemplate);
            }
        } finally {
            lock.unlock();
        }
    }

    public void create(String name) {
        lock.lock();
        try {
            final var workflowTemplate = new WorkflowTemplate();
            workflowTemplate.setName(name);
            workflowTemplate.setWorkflowDefinitions(List.of());
            cache.put(applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(workflowTemplate).getId(),
                workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    public void delete(@NonNull WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            cache.evict(workflowTemplate.getId());
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).unschedule(workflowTemplate);
            }
            applicationManager.getBeanOfType(WorkflowTemplateRepository.class).delete(workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    public void updateActivation(@NonNull WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            workflowTemplate.toggleOnEnabling();
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).unschedule(workflowTemplate);
            } else {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).schedule(workflowTemplate);
            }
            applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    public void updateDefinition(@NonNull WorkflowTemplate workflowTemplate, Stream<WorkflowDefinition> newDefinitions) {
        lock.lock();
        try {
            workflowTemplate.setWorkflowDefinitions(newDefinitions.collect(Collectors.toList()));
            applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    public void updateParameter(@NonNull WorkflowTemplate workflowTemplate, Stream<WorkflowParameter> newParameters) {
        lock.lock();
        try {
            workflowTemplate.setWorkflowParameters(newParameters.collect(Collectors.toList()));
            applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    public void updateNameAndCron(@NonNull WorkflowTemplate workflowTemplate, String newName, String newCron) {
        lock.lock();
        try {
            workflowTemplate.setName(newName);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).unschedule(workflowTemplate);
            }
            workflowTemplate.setCron(newCron);
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).schedule(workflowTemplate);
            }
            applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    public void scheduleNow(@NonNull WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).scheduleNowAsync(workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowTemplateFilter createFilter() {
        return new WorkflowTemplateFilter();
    }

}
