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
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.exception.WorkflowDuplicateException;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.myworkflows.repository.WorkflowTemplateRepository;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.SLEEP_SCRIPT;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.definition;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.parameter;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.template;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowTemplateServiceTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private InternalCacheManager internalCacheManager;

    @Mock
    private WorkflowTemplateSchedulerService workflowTemplateSchedulerService;

    @Mock
    private WorkflowTemplateRepository workflowTemplateRepository;

    @Mock
    private WorkflowDefinitionRepository workflowDefinitionRepository;

    @Mock
    private WorkflowParameterRepository workflowParameterRepository;

    @Mock
    private WorkflowParameterService workflowParameterService;

    private final InternalCache cache = new InternalCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName());

    private WorkflowTemplateService workflowTemplateService;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(anyString())).thenReturn(cache);
        lenient().when(applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)).thenReturn(workflowTemplateSchedulerService);
        workflowTemplateService = new WorkflowTemplateService(applicationManager);
    }

    @Test
    public void whenTickedTemplateTakesOverWhatWasSavedThenItIsScheduledAnew() {
        // given a template which is scheduled, and what an import saved of it
        final var fetch = definition(1, "Fetch", SLEEP_SCRIPT);
        final var send = definition(2, "Send", SLEEP_SCRIPT);
        final var retries = parameter("retries", WorkflowParameterType.INT, "3");
        final var cached = template(8, "Report", "0 0 6 * * *", true, List.of(fetch), List.of());
        final var saved = template(8, "Report", "0 30 6 * * *", false, List.of(fetch, send), List.of(retries));

        // when
        workflowTemplateService.applyInternalUpdate(cached, saved);

        // then it runs what was saved, at the time which was saved
        assertEquals("0 30 6 * * *", cached.getCron());
        assertEquals(List.of(fetch, send), cached.getWorkflowDefinitions());
        assertEquals(List.of(retries), cached.getWorkflowParameters());
        // with lists of its own: what is saved later of one is no change of the other
        assertNotSame(saved.getWorkflowDefinitions(), cached.getWorkflowDefinitions());
        // and it keeps its activation, as an import does not touch it
        assertTrue(cached.isEnabled());
        final var order = inOrder(workflowTemplateSchedulerService);
        order.verify(workflowTemplateSchedulerService).unschedule(cached);
        order.verify(workflowTemplateSchedulerService).schedule(cached);
    }

    @Test
    public void whenPausedTemplateTakesOverWhatWasSavedThenItStaysOutOfTheSchedule() {
        // given
        final var cached = template(8, "Report", null, false, List.of(), List.of());
        final var saved = template(8, "Report", "0 30 6 * * *", true, List.of(), List.of());

        // when
        workflowTemplateService.applyInternalUpdate(cached, saved);

        // then
        assertEquals("0 30 6 * * *", cached.getCron());
        assertFalse(cached.isEnabled());
        verifyNoInteractions(workflowTemplateSchedulerService);
    }

    @Test
    public void whenTemplateIsUpdatedThenItIsScheduledWithWhatItBecameAndSavedOnce() {
        // given a template which is scheduled
        final var fetch = definition(1, "Fetch", SLEEP_SCRIPT);
        final var retries = parameter("retries", WorkflowParameterType.INT, "3");
        final var template = template(8, "Report", "0 0 6 * * *", true, List.of(), List.of());
        when(applicationManager.getBeanOfType(WorkflowTemplateRepository.class)).thenReturn(workflowTemplateRepository);
        final var scheduledCron = new AtomicReference<String>();
        doAnswer(_ -> {
            scheduledCron.set(template.getCron());
            return null;
        }).when(workflowTemplateSchedulerService).schedule(template);

        // when
        workflowTemplateService.update(template, "Weekly report", "0 0 6 * * MON", true, List.of(fetch), List.of(retries));

        // then
        assertEquals("Weekly report", template.getName());
        assertEquals(List.of(fetch), template.getWorkflowDefinitions());
        assertEquals(List.of(retries), template.getWorkflowParameters());
        assertTrue(template.isEnabled());
        assertEquals("0 0 6 * * MON", scheduledCron.get());
        final var order = inOrder(workflowTemplateSchedulerService, workflowTemplateRepository);
        order.verify(workflowTemplateSchedulerService).unschedule(template);
        order.verify(workflowTemplateSchedulerService).schedule(template);
        order.verify(workflowTemplateRepository).save(template);
    }

    @Test
    public void whenTemplateIsDuplicatedThenTheCopyRunsTheSameDefinitionsWithParametersOfItsOwnAndIsPaused() {
        // given a scheduled template, with a parameter it shares and one which is its own
        final var fetch = definition(1, "Fetch", SLEEP_SCRIPT);
        final var send = definition(2, "Send", SLEEP_SCRIPT);
        final var host = parameter("smtp.host", WorkflowParameterType.STR, "mail.local");
        final var timeout = parameter("timeout.42", WorkflowParameterType.INT, "30");
        final var report = cache(template(42, "Report", "0 30 6 * * MON-FRI", true, List.of(fetch, send), List.of(host, timeout)));
        // and what the database holds of what it is made of
        final var savedFetch = definition(1, "Fetch", SLEEP_SCRIPT);
        final var savedSend = definition(2, "Send", SLEEP_SCRIPT);
        final var savedHost = parameter("smtp.host", WorkflowParameterType.STR, "mail.local");
        whenSavedAre(List.of(savedFetch, savedSend), List.of(savedHost));
        when(workflowParameterRepository.save(any())).thenAnswer(invocation -> invocation.getArgument(0));
        when(workflowParameterService.getAllItems()).thenAnswer(_ -> Stream.empty());

        // when
        final var copy = workflowTemplateService.duplicate(report, "Report v2", true);

        // then it runs what the database holds, in the same order, and its own parameter is named after its id
        assertEquals(57, copy.getId());
        assertEquals("Report v2", copy.getName());
        assertEquals("0 30 6 * * MON-FRI", copy.getCron());
        assertSame(savedFetch, copy.getWorkflowDefinitions().get(0));
        assertSame(savedSend, copy.getWorkflowDefinitions().get(1));
        assertSame(savedHost, copy.getWorkflowParameters().get(0));
        final var ownParameter = copy.getWorkflowParameters().get(1);
        assertEquals("timeout.57", ownParameter.getName());
        assertEquals(WorkflowParameterType.INT, ownParameter.getType());
        assertEquals("30", ownParameter.getValue());
        // and it is paused, so it is not scheduled
        assertFalse(copy.isEnabled());
        verifyNoInteractions(workflowTemplateSchedulerService);
        // the caches know both of what was saved
        verify(workflowParameterService).create(ownParameter, false);
        assertSame(copy, cache.get(57, WorkflowTemplate.class));
        // and the original is left as it was
        assertTrue(report.isEnabled());
        assertEquals(List.of(host, timeout), report.getWorkflowParameters());
        assertEquals("30", timeout.getValue());
    }

    @Test
    public void whenTemplateIsDuplicatedWithoutItsOwnParametersThenTheCopyHasOnlyTheSharedOnes() {
        // given
        final var host = parameter("smtp.host", WorkflowParameterType.STR, "mail.local");
        final var report = cache(template(42, "Report", null, false, List.of(), List.of(host, parameter("timeout.42", WorkflowParameterType.INT, "30"))));
        whenSavedAre(List.of(), List.of(host));

        // when
        final var copy = workflowTemplateService.duplicate(report, "Report v2", false);

        // then
        assertEquals(List.of(host), copy.getWorkflowParameters());
        assertNull(copy.getCron());
        verify(workflowParameterRepository, never()).save(any());
        verifyNoInteractions(workflowParameterService);
    }

    @Test
    public void whenNameIsTakenOrOriginalChangedThenNothingIsDuplicated() {
        // given a template whose definition is not in the database anymore
        final var report = cache(template(42, "Report", null, false, List.of(definition(1, "Fetch", SLEEP_SCRIPT)), List.of()));
        whenSavedAre(List.of(), List.of());

        // when and then
        final var taken = assertThrows(WorkflowDuplicateException.class, () -> workflowTemplateService.duplicate(report, " Report ", true));
        assertEquals(WorkflowDuplicateException.Reason.NAME_TAKEN, taken.getReason());
        verify(workflowTemplateRepository, never()).save(any());
        final var changed = assertThrows(WorkflowDuplicateException.class, () -> workflowTemplateService.duplicate(report, "Report v2", true));
        assertEquals(WorkflowDuplicateException.Reason.ORIGINAL_CHANGED, changed.getReason());
        // the transaction is rolled back, and the cache was not told about the copy
        assertEquals(1, cache.size());
    }

    @Test
    public void whenTemplateIsUpdatedAsDisabledThenItLeavesTheScheduleAndEntersItAgainOnceEnabled() {
        // given
        final var template = template(8, "Report", "0 0 6 * * *", true, List.of(), List.of());
        when(applicationManager.getBeanOfType(WorkflowTemplateRepository.class)).thenReturn(workflowTemplateRepository);

        // when
        workflowTemplateService.update(template, "Report", "0 0 6 * * *", false, List.of(), List.of());

        // then
        assertFalse(template.isEnabled());
        verify(workflowTemplateSchedulerService).unschedule(template);
        verify(workflowTemplateSchedulerService, never()).schedule(template);

        // when
        workflowTemplateService.update(template, "Report", "0 0 6 * * *", true, List.of(), List.of());

        // then a template which was not scheduled is not taken out of the schedule
        assertTrue(template.isEnabled());
        verify(workflowTemplateSchedulerService, times(1)).unschedule(template);
        verify(workflowTemplateSchedulerService).schedule(template);
        verify(workflowTemplateRepository, times(2)).save(template);
    }

    private WorkflowTemplate cache(WorkflowTemplate workflowTemplate) {
        cache.put(workflowTemplate.getId(), workflowTemplate);
        return workflowTemplate;
    }

    /**
     * What the database holds, and a save which gives a new template the id 57.
     */
    private void whenSavedAre(List<WorkflowDefinition> definitions, List<WorkflowParameter> parameters) {
        lenient().when(applicationManager.getBeanOfType(WorkflowTemplateRepository.class)).thenReturn(workflowTemplateRepository);
        lenient().when(applicationManager.getBeanOfType(WorkflowDefinitionRepository.class)).thenReturn(workflowDefinitionRepository);
        lenient().when(applicationManager.getBeanOfType(WorkflowParameterRepository.class)).thenReturn(workflowParameterRepository);
        lenient().when(applicationManager.getBeanOfType(WorkflowParameterService.class)).thenReturn(workflowParameterService);
        lenient().when(workflowTemplateRepository.save(any())).thenAnswer(invocation -> {
            final WorkflowTemplate workflowTemplate = invocation.getArgument(0);
            if (workflowTemplate.getId() != null) {
                return workflowTemplate;
            }
            final var saved = spy(workflowTemplate);
            lenient().doReturn(57).when(saved).getId();
            return saved;
        });
        lenient().when(workflowDefinitionRepository.findById(any())).thenAnswer(invocation -> definitions.stream()
            .filter(definition -> definition.getId().equals(invocation.getArgument(0)))
            .findFirst());
        lenient().when(workflowParameterRepository.findById(any())).thenAnswer(invocation -> parameters.stream()
            .filter(parameter -> parameter.getName().equals(invocation.getArgument(0)))
            .findFirst());
    }

}
