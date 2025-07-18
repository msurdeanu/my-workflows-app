package org.myworkflows.service;

import lombok.NonNull;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_TEMPLATE_NAME;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public class WorkflowTemplateService extends CacheableDataService<WorkflowTemplate, WorkflowTemplateFilter> implements ServiceCreator<WorkflowTemplate> {

    public WorkflowTemplateService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_TEMPLATE);
    }

    @CachePut(cacheNames = WORKFLOW_TEMPLATE_NAME, key = "#result.id")
    public WorkflowTemplate create(WorkflowTemplate workflowTemplate, boolean requiresPersistence) {
        lock.lock();
        try {
            assert cache != null;
            ofNullable(cache.get(workflowTemplate.getId(), WorkflowTemplate.class))
                .filter(WorkflowTemplate::isEnabledForScheduling)
                .ifPresent(oldItem -> applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .unschedule(workflowTemplate));
            if (requiresPersistence) {
                applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(workflowTemplate);
            }
            if (workflowTemplate.isEnabled()) {
                applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)
                    .schedule(workflowTemplate);
            }
        } finally {
            lock.unlock();
        }

        return workflowTemplate;
    }

    @CacheEvict(cacheNames = WORKFLOW_TEMPLATE_NAME, key = "#workflowTemplate.id")
    public void delete(WorkflowTemplate workflowTemplate) {
        lock.lock();
        try {
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
            final var workflowTemplateSchedulerService = applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class);
            if (workflowTemplate.isEnabled()) {
                workflowTemplateSchedulerService.schedule(workflowTemplate);
            } else {
                workflowTemplateSchedulerService.unschedule(workflowTemplate);
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

    public void propagateInternalUpdate(WorkflowDefinition workflowDefinition) {
        assert cache != null;
        cache.getAllValues().stream()
            .filter(object -> object instanceof WorkflowTemplate)
            .map(WorkflowTemplate.class::cast)
            .forEach(workflowTemplate -> workflowTemplate.applyUpdateOnWorkflowDefinitionIfNeeded(workflowDefinition));
    }

    public void propagateInternalUpdate(WorkflowParameter workflowParameter) {
        assert cache != null;
        cache.getAllValues().stream()
            .filter(object -> object instanceof WorkflowTemplate)
            .map(WorkflowTemplate.class::cast)
            .forEach(workflowTemplate -> workflowTemplate.applyUpdateOnWorkflowParameterIfNeeded(workflowParameter));
    }

    @Override
    protected WorkflowTemplateFilter createFilter() {
        return new WorkflowTemplateFilter();
    }

}
