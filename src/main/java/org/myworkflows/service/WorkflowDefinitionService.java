package org.myworkflows.service;

import jakarta.transaction.Transactional;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.filter.WorkflowDefinitionFilter;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.myworkflows.util.DuplicateNameUtil;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_DEFINITION_NAME;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
public class WorkflowDefinitionService extends CacheableDataService<WorkflowDefinition, WorkflowDefinitionFilter>
    implements ServiceCreator<WorkflowDefinition> {

    public WorkflowDefinitionService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_DEFINITION);
    }

    @Override
    @CachePut(cacheNames = WORKFLOW_DEFINITION_NAME, key = "#result.id")
    public WorkflowDefinition create(WorkflowDefinition workflowDefinition, boolean requiresPersistence) {
        if (requiresPersistence) {
            lock.lock();
            try {
                applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
            } finally {
                lock.unlock();
            }
        }

        return workflowDefinition;
    }

    @Transactional
    public int delete(WorkflowDefinition workflowDefinition) {
        lock.lock();
        try {
            final var rowsAffected = applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).deleteIfNotUsed(workflowDefinition.getId());
            if (rowsAffected > 0) {
                cache.evict(workflowDefinition.getId());
            }
            return rowsAffected;
        } finally {
            lock.unlock();
        }
    }

    @Transactional
    public void updateDefinition(WorkflowDefinition workflowDefinition, String newScript) {
        lock.lock();
        try {
            workflowDefinition.setScript(toObject(newScript, WorkflowDefinitionScript.class));
            applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
            applicationManager.getBeanOfType(WorkflowTemplateService.class).propagateInternalUpdate(workflowDefinition);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Gives a definition another name and another script in one change, which the templates that run it follow.
     */
    @Transactional
    public void update(WorkflowDefinition workflowDefinition, String newName, WorkflowDefinitionScript newScript) {
        lock.lock();
        try {
            workflowDefinition.setName(newName);
            workflowDefinition.setScript(newScript);
            applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
            applicationManager.getBeanOfType(WorkflowTemplateService.class).propagateInternalUpdate(workflowDefinition);
        } finally {
            lock.unlock();
        }
    }

    @Transactional
    public void updateName(WorkflowDefinition workflowDefinition, String newName) {
        lock.lock();
        try {
            workflowDefinition.setName(newName);
            applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(workflowDefinition);
            applicationManager.getBeanOfType(WorkflowTemplateService.class).propagateInternalUpdate(workflowDefinition);
        } finally {
            lock.unlock();
        }
    }

    public boolean isNameTaken(String name) {
        return getAllItems().anyMatch(workflowDefinition -> workflowDefinition.getName().equals(name));
    }

    /**
     * @return a name for a copy of the given definition, which no definition has yet.
     */
    public String suggestCopyName(WorkflowDefinition workflowDefinition) {
        return DuplicateNameUtil.suggest(workflowDefinition.getName(), this::isNameTaken);
    }

    /**
     * Saves a new definition with the script the given one was saved with. The templates which run the given one go on
     * with it.
     *
     * @throws WorkflowDuplicateException when the name is missing or taken.
     */
    public WorkflowDefinition duplicate(WorkflowDefinition workflowDefinition, String name) {
        // both share the script: it is never changed in place, as a run works on a copy of it and a save replaces it
        return createCopy(name, workflowDefinition.getScript());
    }

    /**
     * Saves the given script as a new definition: what the editor holds, kept apart from the definition it was opened
     * with.
     *
     * @throws WorkflowRuntimeException when the script cannot be read.
     * @throws WorkflowDuplicateException when the name is missing or taken.
     */
    public WorkflowDefinition duplicate(String name, String script) {
        return createCopy(name, toObject(script, WorkflowDefinitionScript.class));
    }

    private WorkflowDefinition createCopy(String name, WorkflowDefinitionScript script) {
        lock.lock();
        try {
            // checked and saved under the same lock, so that two copies never take the same name
            final var copyName = DuplicateNameUtil.requireFree(name, this::isNameTaken);
            final var copy = applicationManager.getBeanOfType(WorkflowDefinitionRepository.class).save(WorkflowDefinition.of(copyName, script));
            cache.put(copy.getId(), copy);
            return copy;
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected WorkflowDefinitionFilter createFilter() {
        return new WorkflowDefinitionFilter();
    }

}
