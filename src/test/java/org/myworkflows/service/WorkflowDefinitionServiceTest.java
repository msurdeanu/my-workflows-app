package org.myworkflows.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.cache.CacheNameEnum;
import org.myworkflows.cache.InternalCache;
import org.myworkflows.cache.InternalCacheManager;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.repository.WorkflowDefinitionRepository;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.SLEEP_SCRIPT;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.definition;

/**
 * @author Mihai Surdeanu
 * @since 2.2
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowDefinitionServiceTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private InternalCacheManager internalCacheManager;

    @Mock
    private WorkflowDefinitionRepository workflowDefinitionRepository;

    private final InternalCache cache = new InternalCache(CacheNameEnum.WORKFLOW_DEFINITION.getName());

    private final AtomicInteger nextId = new AtomicInteger(30);

    private WorkflowDefinitionService workflowDefinitionService;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(anyString())).thenReturn(cache);
        lenient().when(applicationManager.getBeanOfType(WorkflowDefinitionRepository.class)).thenReturn(workflowDefinitionRepository);
        // a save gives a new definition its id
        lenient().when(workflowDefinitionRepository.save(any())).thenAnswer(invocation -> {
            final var saved = spy(invocation.<WorkflowDefinition>getArgument(0));
            doReturn(nextId.incrementAndGet()).when(saved).getId();
            return saved;
        });
        workflowDefinitionService = new WorkflowDefinitionService(applicationManager);
    }

    @Test
    public void whenDefinitionIsDuplicatedThenTheCopyHasItsScriptUnderANameOfItsOwn() {
        // given
        final var backup = cache(definition(1, "Backup", SLEEP_SCRIPT));
        cache(definition(2, "Backup (copy)", "commands: []"));

        // when
        final var name = workflowDefinitionService.suggestCopyName(backup);
        final var copy = workflowDefinitionService.duplicate(backup, " " + name + " ");

        // then
        assertEquals("Backup (copy 2)", copy.getName());
        assertSame(backup.getScript(), copy.getScript());
        assertSame(copy, cache.get(31, WorkflowDefinition.class));
        // and the original is left as it was
        assertEquals("Backup", backup.getName());
    }

    @Test
    public void whenNameIsTakenThenNoCopyIsSaved() {
        // given
        final var backup = cache(definition(1, "Backup", SLEEP_SCRIPT));

        // when and then
        final var exception = assertThrows(WorkflowDuplicateException.class, () -> workflowDefinitionService.duplicate(backup, "Backup"));
        assertEquals(WorkflowDuplicateException.Reason.NAME_TAKEN, exception.getReason());
        verify(workflowDefinitionRepository, never()).save(any());
    }

    @Test
    public void whenEditorIsSavedAsCopyThenItsScriptIsReadFirst() {
        // when
        final var copy = workflowDefinitionService.duplicate("Backup v2", SLEEP_SCRIPT);

        // then
        assertEquals("Backup v2", copy.getName());
        assertEquals(1, copy.getScript().getCommands().size());
        assertThrows(WorkflowRuntimeException.class, () -> workflowDefinitionService.duplicate("Broken", "commands: ["));
        verify(workflowDefinitionRepository).save(any());
    }

    private WorkflowDefinition cache(WorkflowDefinition workflowDefinition) {
        cache.put(workflowDefinition.getId(), workflowDefinition);
        return workflowDefinition;
    }

}
