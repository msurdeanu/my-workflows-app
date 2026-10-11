package org.myworkflows.domain;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowDefinitionScriptRunnableTest {

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private EventBroadcaster eventBroadcaster;

    @BeforeEach
    public void init() {
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
    }

    @Test
    public void whenTemplateIsRunByUserThenRunBelongsToThatUser() {
        // given
        final var workflowTemplate = WorkflowTemplate.of("test", List.of(), List.of());

        // when
        new WorkflowDefinitionScriptRunnable(applicationManager, workflowTemplate, 7).run();

        // then
        assertEquals(7, captureSubmittedRun().getUserId());
    }

    @Test
    public void whenTemplateIsRunByCronThenRunBelongsToNobody() {
        // given
        final var workflowTemplate = WorkflowTemplate.of("test", List.of(), List.of());

        // when
        new WorkflowDefinitionScriptRunnable(applicationManager, workflowTemplate, null).run();

        // then
        assertNull(captureSubmittedRun().getUserId());
    }

    private WorkflowRun captureSubmittedRun() {
        final var eventCaptor = ArgumentCaptor.forClass(WorkflowDefinitionOnSubmitEvent.class);
        verify(eventBroadcaster).broadcast(eventCaptor.capture());
        return eventCaptor.getValue().workflowRun();
    }

}
