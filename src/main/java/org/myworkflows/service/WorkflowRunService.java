package org.myworkflows.service;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.WorkflowRunApproval;
import org.myworkflows.domain.WorkflowRunPause;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.domain.filter.WorkflowRunFilter;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.repository.UserRepository;
import org.myworkflows.repository.WorkflowRunRepository;
import org.myworkflows.util.SecurityUtil;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import static java.util.Optional.ofNullable;
import static org.myworkflows.cache.CacheNameEnum.WORKFLOW_RUN_NAME;
import static org.springframework.data.domain.PageRequest.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Service
public class WorkflowRunService extends CacheableDataService<WorkflowRun, WorkflowRunFilter> implements ServiceCreator<WorkflowRun> {

    private final Map<Integer, Optional<String>> usernames = new ConcurrentHashMap<>();

    private final SettingProvider settingProvider;
    private final InternalCache templateCache;

    public WorkflowRunService(ApplicationManager applicationManager) {
        super(applicationManager, CacheNameEnum.WORKFLOW_RUN);
        settingProvider = applicationManager.getBeanOfType(SettingProvider.class);
        templateCache = (InternalCache) applicationManager.getBeanOfType(InternalCacheManager.class)
            .getCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName());
    }

    public Optional<WorkflowTemplate> findWorkflowTemplate(WorkflowRun workflowRun) {
        return ofNullable(workflowRun.getWorkflowTemplateId())
            .map(id -> templateCache.get(id, WorkflowTemplate.class));
    }

    public Optional<String> findUsername(WorkflowRun workflowRun) {
        // A user can be neither renamed nor deleted, so a name is read once and kept for the following lookups.
        return ofNullable(workflowRun.getUserId())
            .flatMap(id -> usernames.computeIfAbsent(id, key -> applicationManager.getBeanOfType(UserRepository.class)
                .findById(key)
                .map(User::getUsername)));
    }

    @Override
    @CachePut(cacheNames = WORKFLOW_RUN_NAME, key = "#result.id")
    public WorkflowRun create(WorkflowRun workflowRun, boolean requiresPersistence) {
        if (requiresPersistence) {
            lock.lock();
            try {
                applicationManager.getBeanOfType(WorkflowRunRepository.class).save(workflowRun);
            } finally {
                lock.unlock();
            }
        }

        return workflowRun;
    }

    @CacheEvict(cacheNames = WORKFLOW_RUN_NAME, key = "#workflowRun.id")
    public void delete(WorkflowRun workflowRun) {
        lock.lock();
        try {
            applicationManager.getBeanOfType(WorkflowRunRepository.class).delete(workflowRun);
        } finally {
            lock.unlock();
        }
    }

    public boolean replay(WorkflowRun workflowRun) {
        final var userId = SecurityUtil.findCurrentUserId().orElse(null);
        lock.lock();
        try {
            if (!workflowRun.isReplayableBy(userId)) {
                log.warn("Workflow run '{}' is not replayed, because it was not triggered by the user asking for it.", workflowRun.getId());
                return false;
            }
            workflowRun.prepareForReplay();
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                    .workflowRun(workflowRun)
                    .workflowDefinitionScript(workflowRun.getScript())
                    .build());
            return true;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Approves or rejects, in the name of the current user, an approval the given run asked for. The approval is the one
     * the user was shown, which is not always the one the run waits for by the time they decide.
     *
     * @return whether the decision was taken into account. It is not when the run does not wait for this approval
     *     anymore: somebody else decided, the approval timed out or the run was cancelled.
     */
    public boolean decideApproval(WorkflowRun workflowRun, WorkflowRunApproval approval, boolean approved) {
        final var username = SecurityUtil.findCurrentAccount().map(UserAccountDetails::getUsername).orElse(null);
        if (username == null) {
            return false;
        }

        final var decided = approved ? approval.approve(username) : approval.reject(username);
        if (decided) {
            log.info("Workflow run '{}' was {} by '{}' at command '{}'.", workflowRun.getId(), approved ? "approved" : "rejected", username,
                approval.getCommandName());
        }
        return decided;
    }

    /**
     * Tells a paused run how to go on, or to stop, in the name of the current user. The pause is the one the user was
     * shown, which is not always the one the run waits at by the time they answer.
     *
     * @return whether the run follows. It does not when it is not paused there anymore - it was told something else, the
     *     pause timed out or the run was cancelled - or when the user is not the one who started the run.
     */
    public boolean resume(WorkflowRun workflowRun, WorkflowRunPause pause, WorkflowRunPause.Resume resume) {
        if (!isStartedByCurrentUser(workflowRun)) {
            log.warn("Workflow run '{}' stays paused, because it was not started by the user who tells it how to go on.", workflowRun.getId());
            return false;
        }

        final var resumed = pause.resume(resume);
        if (resumed) {
            log.debug("Workflow run '{}', paused before command '{}', was told to {} by '{}'.", workflowRun.getId(), pause.getCommandName(),
                resume.action().name().toLowerCase(), resume.username());
        }
        return resumed;
    }

    /**
     * Starts a run which is over again from the given command, with the given script and with everything the run held
     * when it ended. It is what a replay does without its limits: the run may have passed, any command can be the one it
     * starts from and the script is the one somebody just wrote. It takes the run as the page which started it keeps it,
     * though: a run read from the database has lost whatever it held that could not be saved.
     *
     * @param script the script to run, which the run owns from now on.
     * @param breakpoints the commands to pause before, when the run is debugged.
     * @return whether the run was started again. It is not unless it is over - a run which was just started again is not,
     *     so it cannot be started twice - or when the user is not the one who started it.
     */
    public boolean runFrom(WorkflowRun workflowRun, int commandIndex, WorkflowDefinitionScript script, Set<Integer> breakpoints) {
        lock.lock();
        try {
            if (!workflowRun.isCompleted() || !isStartedByCurrentUser(workflowRun)) {
                log.warn("Workflow run '{}' is not started again: it is not over, or it was not started by the user asking for it.",
                    workflowRun.getId());
                return false;
            }
            workflowRun.prepareForReplay();
            workflowRun.moveTo(commandIndex);
            ofNullable(workflowRun.getDebugger()).ifPresent(debugger -> debugger.goOn(commandIndex, false, breakpoints));
            applicationManager.getBeanOfType(EventBroadcaster.class)
                .broadcast(WorkflowDefinitionOnSubmitEvent.builder()
                    .workflowRun(workflowRun)
                    .workflowDefinitionScript(script)
                    .build());
            return true;
        } finally {
            lock.unlock();
        }
    }

    public void deleteOldEntriesIfNeeded(boolean force) {
        final var workflowRunMaxSize = settingProvider.getOrDefault("workflowRunMaxSize", 250);
        if (!force && cache.size() < workflowRunMaxSize) {
            return;
        }
        final var start = System.currentTimeMillis();
        final var workflowRunRepository = applicationManager.getBeanOfType(WorkflowRunRepository.class);
        final var oldestCutoffDate = workflowRunRepository.findOldestCutoffDate(of(workflowRunMaxSize, 1));
        if (oldestCutoffDate.size() != 1) {
            return;
        }
        final var eldestEntries = workflowRunRepository.findOldestEntriesByCutoff(oldestCutoffDate.getFirst());
        if (!eldestEntries.isEmpty()) {
            workflowRunRepository.deleteAllById(eldestEntries);
        }
        if (log.isDebugEnabled()) {
            log.debug("{} run(s) deleted from database in {} ms.", eldestEntries.size(),
                System.currentTimeMillis() - start);
        }
    }

    @Override
    protected WorkflowRunFilter createFilter() {
        return new WorkflowRunFilter();
    }

    private boolean isStartedByCurrentUser(WorkflowRun workflowRun) {
        return workflowRun.getUserId() != null && workflowRun.getUserId().equals(SecurityUtil.findCurrentUserId().orElse(null));
    }

}
