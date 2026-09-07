package org.myworkflows.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.ApplicationManager;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.event.WorkflowDefinitionOnSubmitEvent;
import org.myworkflows.serializer.SerializerFactory;

import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.myworkflows.serializer.SerializerFactory.toObject;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowScriptServiceTest {

    private static final String SCRIPT = """
        commands:
        - class: nothing
          name: Nothing command
          inputs:
          - name: greeting
            value: $$(MY_VALUE)
        """;

    @Mock
    private ApplicationManager applicationManager;

    @Mock
    private EventBroadcaster eventBroadcaster;

    @Mock
    private WorkflowPlaceholderService workflowPlaceholderService;

    @Test
    public void whenTheSameScriptIsRunTwiceThenPlaceholdersAreResolvedAgainstTheCurrentValues() throws Exception {
        // given a single script instance, as it is shared by the internal cache across runs
        final var sharedScript = toObject(SCRIPT, WorkflowDefinitionScript.class);
        final var executorService = Executors.newSingleThreadExecutor();
        final var workflowScriptService = new WorkflowScriptService(executorService, applicationManager);
        when(applicationManager.getBeanOfType(EventBroadcaster.class)).thenReturn(eventBroadcaster);
        when(applicationManager.getBeanOfType(WorkflowPlaceholderService.class)).thenReturn(workflowPlaceholderService);

        // when the placeholder value changes between the two runs
        when(workflowPlaceholderService.getAllAsMap()).thenReturn(Map.of("MY_VALUE", "first"));
        final var firstRun = run(workflowScriptService, sharedScript, executorService);
        when(workflowPlaceholderService.getAllAsMap()).thenReturn(Map.of("MY_VALUE", "second"));
        final var secondRun = run(workflowScriptService, sharedScript, executorService);
        executorService.shutdown();
        assertTrue(executorService.awaitTermination(30, TimeUnit.SECONDS));

        // then each run sees the value that was current when it started
        assertEquals("first", firstRun.getCache().get("greeting"));
        assertEquals("second", secondRun.getCache().get("greeting"));
        // and the shared script is left untouched, so it can still be edited and persisted as authored
        assertTrue(SerializerFactory.toString(sharedScript, "").contains("$$(MY_VALUE)"));
    }

    private WorkflowRun run(WorkflowScriptService workflowScriptService, WorkflowDefinitionScript script,
                            ExecutorService executorService) throws Exception {
        final var workflowRun = new WorkflowRun();
        workflowScriptService.onEventReceived(WorkflowDefinitionOnSubmitEvent.builder()
            .workflowRun(workflowRun)
            .workflowDefinitionScript(script)
            .build());
        // the run is submitted to the executor, so wait for it to drain before looking at the cache
        executorService.submit(() -> { }).get(30, TimeUnit.SECONDS);
        return workflowRun;
    }

}
