package org.myworkflows.service;

import jakarta.transaction.Transactional;
import lombok.NonNull;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.filter.WorkflowTemplateFilter;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.myworkflows.util.DuplicateNameUtil;
import org.myworkflows.util.SecurityUtil;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_TEMPLATE_NAME;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.definitionsOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.isOwn;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.ownNameOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.parametersOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.storedNameOf;

/**
 * @author Mihai Surdeanu
 * @since 1.0
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

    /**
     * Replaces everything a template is made of in one change, saved once. The template is scheduled anew, as its cron
     * and its activation may be other ones.
     */
    public void update(@NonNull WorkflowTemplate workflowTemplate, String newName, String newCron, boolean enabled,
                       List<WorkflowDefinition> newDefinitions, List<WorkflowParameter> newParameters) {
        lock.lock();
        try {
            final var workflowTemplateSchedulerService = applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class);
            if (workflowTemplate.isEnabled()) {
                workflowTemplateSchedulerService.unschedule(workflowTemplate);
            }
            workflowTemplate.setName(newName);
            workflowTemplate.setCron(newCron);
            if (workflowTemplate.isEnabled() != enabled) {
                workflowTemplate.toggleOnEnabling();
            }
            workflowTemplate.setWorkflowDefinitions(new ArrayList<>(newDefinitions));
            workflowTemplate.setWorkflowParameters(new ArrayList<>(newParameters));
            if (workflowTemplate.isEnabled()) {
                workflowTemplateSchedulerService.schedule(workflowTemplate);
            }
            applicationManager.getBeanOfType(WorkflowTemplateRepository.class).save(workflowTemplate);
        } finally {
            lock.unlock();
        }
    }

    public void scheduleNow(@NonNull WorkflowTemplate workflowTemplate) {
        // The user is resolved here, on the calling thread, because the run itself is created by the scheduler.
        final var userId = SecurityUtil.findCurrentUserId().orElse(null);
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class).scheduleNowAsync(workflowTemplate, userId);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Brings a template of the cache in line with what was saved of it by somebody else - an import, which saves all
     * it brings in one transaction - without saving it again. The template is scheduled anew, as its cron may be another.
     */
    public void applyInternalUpdate(@NonNull WorkflowTemplate workflowTemplate, @NonNull WorkflowTemplate savedTemplate) {
        lock.lock();
        try {
            final var workflowTemplateSchedulerService = applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class);
            if (workflowTemplate.isEnabled()) {
                workflowTemplateSchedulerService.unschedule(workflowTemplate);
            }
            workflowTemplate.setCron(savedTemplate.getCron());
            workflowTemplate.setWorkflowDefinitions(new ArrayList<>(ofNullable(savedTemplate.getWorkflowDefinitions()).orElse(List.of())));
            workflowTemplate.setWorkflowParameters(new ArrayList<>(ofNullable(savedTemplate.getWorkflowParameters()).orElse(List.of())));
            if (workflowTemplate.isEnabled()) {
                workflowTemplateSchedulerService.schedule(workflowTemplate);
            }
        } finally {
            lock.unlock();
        }
    }

    public boolean isNameTaken(String name) {
        return getAllItems().anyMatch(workflowTemplate -> workflowTemplate.getName().equals(name));
    }

    /**
     * @return a name for a copy of the given template, which no template has yet.
     */
    public String suggestCopyName(WorkflowTemplate workflowTemplate) {
        return DuplicateNameUtil.suggest(workflowTemplate.getName(), this::isNameTaken);
    }

    /**
     * Saves a new template which runs what the given one runs, with the same parameters and cron, all of it in one
     * transaction. The copy is paused, so that it does not run next to the given one before it was looked at.
     *
     * @param withOwnParameters whether the parameters of the given template alone (`timeout.42`) are copied for the new
     *     one (`timeout.57`), with their type and value. Otherwise the copy has none.
     * @throws WorkflowDuplicateException when the name is missing or taken, or when something the given template is
     *     made of is not in the database anymore.
     */
    @Transactional
    public WorkflowTemplate duplicate(@NonNull WorkflowTemplate workflowTemplate, String name, boolean withOwnParameters) {
        lock.lock();
        try {
            final var copyName = DuplicateNameUtil.requireFree(name, this::isNameTaken);
            final var workflowTemplateRepository = applicationManager.getBeanOfType(WorkflowTemplateRepository.class);
            final var workflowDefinitionRepository = applicationManager.getBeanOfType(WorkflowDefinitionRepository.class);
            final var workflowParameterRepository = applicationManager.getBeanOfType(WorkflowParameterRepository.class);
            // saved empty first, as the name of a parameter of its own ends with its id
            final var copy = workflowTemplateRepository.save(WorkflowTemplate.of(copyName, new ArrayList<>(), new ArrayList<>()));
            copy.setCron(workflowTemplate.getCron());
            // what it is made of is read in this transaction: JPA cannot persist a template together with the cached
            // definitions and parameters, which are saved already
            final var definitions = definitionsOf(workflowTemplate).stream()
                .map(workflowDefinition -> workflowDefinitionRepository.findById(workflowDefinition.getId())
                    .orElseThrow(() -> createOriginalChangedException(workflowTemplate)))
                .collect(Collectors.toCollection(ArrayList::new));
            final var parameters = new ArrayList<WorkflowParameter>();
            final var ownParameters = new ArrayList<WorkflowParameter>();
            parametersOf(workflowTemplate).forEach(workflowParameter -> {
                if (!isOwn(workflowTemplate, workflowParameter)) {
                    parameters.add(workflowParameterRepository.findById(workflowParameter.getName())
                        .orElseThrow(() -> createOriginalChangedException(workflowTemplate)));
                } else if (withOwnParameters) {
                    // a parameter left behind by a deleted template with the same id is taken over
                    final var ownParameter = workflowParameterRepository.save(WorkflowParameter.of(
                        storedNameOf(copy.getId(), ownNameOf(workflowTemplate, workflowParameter)),
                        workflowParameter.getType(), workflowParameter.getValue()));
                    parameters.add(ownParameter);
                    ownParameters.add(ownParameter);
                }
            });
            copy.setWorkflowDefinitions(definitions);
            copy.setWorkflowParameters(parameters);
            final var savedCopy = workflowTemplateRepository.save(copy);

            // the caches last, once nothing can go wrong anymore
            final var workflowParameterService = applicationManager.getBeanOfType(WorkflowParameterService.class);
            ownParameters.forEach(ownParameter -> workflowParameterService.getAllItems()
                .filter(cached -> cached.getName().equals(ownParameter.getName()))
                .findFirst()
                .ifPresentOrElse(cached -> cached.setTypeAndValue(ownParameter), () -> workflowParameterService.create(ownParameter, false)));
            cache.put(savedCopy.getId(), savedCopy);
            return savedCopy;
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

    private static WorkflowDuplicateException createOriginalChangedException(WorkflowTemplate workflowTemplate) {
        return new WorkflowDuplicateException(WorkflowDuplicateException.Reason.ORIGINAL_CHANGED, workflowTemplate.getName());
    }

    @Override
    protected WorkflowTemplateFilter createFilter() {
        return new WorkflowTemplateFilter();
    }

}
