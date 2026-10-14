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
import org.myworkflows.domain.WorkflowParameterType;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
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

    private WorkflowTemplateService workflowTemplateService;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(InternalCacheManager.class)).thenReturn(internalCacheManager);
        when(internalCacheManager.getCache(anyString())).thenReturn(new InternalCache(CacheNameEnum.WORKFLOW_TEMPLATE.getName()));
        when(applicationManager.getBeanOfType(WorkflowTemplateSchedulerService.class)).thenReturn(workflowTemplateSchedulerService);
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

}
