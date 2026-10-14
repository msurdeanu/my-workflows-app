package org.myworkflows.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.domain.transfer.WorkflowImportItem;
import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportRequest;
import org.myworkflows.domain.transfer.WorkflowImportReviewer;
import org.myworkflows.domain.transfer.WorkflowTransferContent;
import org.myworkflows.exception.WorkflowTransferException;
import org.myworkflows.exception.WorkflowTransferException.Reason;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.myworkflows.repository.WorkflowPlaceholderRepository;
import org.myworkflows.repository.WorkflowTemplateRepository;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.SLEEP_SCRIPT;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.content;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.definition;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.parameter;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.placeholder;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.requestScript;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.script;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.template;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.DEFINITION;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.PARAMETER;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.PLACEHOLDER;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.TEMPLATE;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
@ExtendWith(MockitoExtension.class)
public final class WorkflowImportWriterTest {

    private static final int FIRST_ID = 100;

    private static final WorkflowTransferContent NOTHING = content(List.of(), List.of(), List.of(), List.of());

    private final AtomicInteger nextId = new AtomicInteger(FIRST_ID);

    @Mock
    private WorkflowTemplateRepository workflowTemplateRepository;

    @Mock
    private WorkflowDefinitionRepository workflowDefinitionRepository;

    @Mock
    private WorkflowParameterRepository workflowParameterRepository;

    @Mock
    private WorkflowPlaceholderRepository workflowPlaceholderRepository;

    private WorkflowImportWriter workflowImportWriter;

    @BeforeEach
    public void init() {
        workflowImportWriter = new WorkflowImportWriter(workflowTemplateRepository, workflowDefinitionRepository, workflowParameterRepository,
            workflowPlaceholderRepository);
        lenient().when(workflowPlaceholderRepository.save(any(WorkflowPlaceholder.class))).thenAnswer(invocation -> invocation.getArgument(0));
        lenient().when(workflowParameterRepository.save(any(WorkflowParameter.class))).thenAnswer(invocation -> invocation.getArgument(0));
        // what is saved for the first time gets its id from the database
        lenient().when(workflowDefinitionRepository.save(any(WorkflowDefinition.class))).thenAnswer(invocation -> {
            final WorkflowDefinition definition = invocation.getArgument(0);
            if (definition.getId() != null) {
                return definition;
            }
            final var saved = spy(definition);
            lenient().doReturn(nextId.getAndIncrement()).when(saved).getId();
            return saved;
        });
        lenient().when(workflowTemplateRepository.save(any(WorkflowTemplate.class))).thenAnswer(invocation -> {
            final WorkflowTemplate template = invocation.getArgument(0);
            if (template.getId() != null) {
                return template;
            }
            final var saved = spy(template);
            lenient().doReturn(nextId.getAndIncrement()).when(saved).getId();
            return saved;
        });
    }

    @Test
    public void whenEverythingIsNewThenEverythingIsCreatedAndTheTemplateIsPaused() {
        // given a file with a template which is ticked, and nothing here
        final var bundle = new WorkflowBundle(1, null,
            List.of(new WorkflowBundle.Placeholder("API.URL", "https://api"), new WorkflowBundle.Placeholder("API.PASSWORD", null),
                new WorkflowBundle.Placeholder("DB.PASSWORD", null)),
            List.of(new WorkflowBundle.Parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com")),
            List.of(new WorkflowBundle.Definition("Fetch", script(SLEEP_SCRIPT))),
            List.of(new WorkflowBundle.Template("Report", " 0 30 6 * * * ", true, List.of("Fetch", "Fetch"), List.of("recipients"),
                List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60")))));
        final var review = new WorkflowImportReviewer(bundle, NOTHING).review();
        final var typedValues = Map.of(WorkflowImportItem.keyOf(PLACEHOLDER, "API.PASSWORD", null), "typed");

        // when
        final var written = workflowImportWriter.write(review, new WorkflowImportRequest(Map.of(), typedValues, true));

        // then a value the file left out is the one which was typed, or none
        assertEquals(List.of("API.URL=https://api", "API.PASSWORD=typed", "DB.PASSWORD="),
            written.getPlaceholders().stream().map(placeholder -> placeholder.getName() + "=" + placeholder.getValue()).toList());

        // the definition and the template got their id, and the template is made of what was saved with it
        final var fetch = written.getDefinitions().getFirst();
        assertEquals(FIRST_ID, fetch.getId());
        final var report = written.getTemplates().getFirst();
        assertEquals(FIRST_ID + 1, report.getId());
        assertEquals("Report", report.getName());
        assertEquals("0 30 6 * * *", report.getCron());
        assertEquals(List.of(fetch), report.getWorkflowDefinitions());
        // its own parameter is saved under a name which ends with its id, after the parameter it shares
        assertEquals(List.of("recipients", "timeout.101"), report.getWorkflowParameters().stream().map(WorkflowParameter::getName).toList());
        assertEquals(List.of("recipients", "timeout.101"), written.getParameters().stream().map(WorkflowParameter::getName).toList());
        assertEquals("60", written.getParameters().get(1).getValue());
        assertEquals(WorkflowParameterType.INT, written.getParameters().get(1).getType());
        // and it is not ticked, though the file has it ticked
        assertFalse(report.isEnabled());

        assertEquals(Map.of("PLACEHOLDER:API.URL", "API.URL", "PLACEHOLDER:API.PASSWORD", "API.PASSWORD", "PLACEHOLDER:DB.PASSWORD", "DB.PASSWORD",
            "PARAMETER:recipients", "recipients", "PARAMETER:Report/timeout", "timeout.101", "DEFINITION:Fetch", "100", "TEMPLATE:Report", "101"),
            written.getTargets());
    }

    @Test
    public void whenNewTemplatesAreNotPausedThenTheyAreWhatTheFileSays() {
        // given
        final var bundle = new WorkflowBundle(1, null, List.of(), List.of(), List.of(),
            List.of(new WorkflowBundle.Template("Ticked", "0 0 6 * * *", true, List.of(), List.of(), List.of()),
                new WorkflowBundle.Template("Unticked", null, false, List.of(), List.of(), List.of())));
        final var review = new WorkflowImportReviewer(bundle, NOTHING).review();

        // when
        final var written = workflowImportWriter.write(review, new WorkflowImportRequest(Map.of(), Map.of(), false));

        // then
        assertTrue(written.getTemplates().getFirst().isEnabled());
        assertFalse(written.getTemplates().get(1).isEnabled());
        assertNull(written.getTemplates().get(1).getCron());
    }

    @Test
    public void whenItemsAreHereThenTheyAreOverwrittenWhereTheyAre() {
        // given a template which is ticked here, with a definition, a parameter it shares and one of its own
        final var fetch = definition(11, "Fetch", SLEEP_SCRIPT);
        final var send = definition(12, "Send", SLEEP_SCRIPT);
        final var recipients = parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com");
        final var ownTimeout = parameter("timeout.8", WorkflowParameterType.INT, "30");
        final var ownRegion = parameter("region.8", WorkflowParameterType.STR, "eu");
        final var report = template(8, "Report", "0 0 6 * * *", true, List.of(send), List.of(recipients, ownTimeout, ownRegion));
        final var here = content(List.of(report), List.of(fetch, send), List.of(recipients, ownTimeout, ownRegion),
            List.of(placeholder("SMTP.HOST", "smtp.test.local"), placeholder("DB.PASSWORD", "mine")));
        when(workflowDefinitionRepository.findById(11)).thenReturn(Optional.of(fetch));
        when(workflowDefinitionRepository.findById(12)).thenReturn(Optional.of(send));
        when(workflowTemplateRepository.findById(8)).thenReturn(Optional.of(report));
        when(workflowParameterRepository.findById("recipients")).thenReturn(Optional.of(recipients));
        when(workflowParameterRepository.findById("region.8")).thenReturn(Optional.of(ownRegion));

        final var newScript = script(requestScript("https://api"));
        final var bundle = new WorkflowBundle(1, null,
            List.of(new WorkflowBundle.Placeholder("SMTP.HOST", "smtp.prod.local"), new WorkflowBundle.Placeholder("DB.PASSWORD", null)),
            List.of(), List.of(new WorkflowBundle.Definition("Fetch", newScript)),
            List.of(new WorkflowBundle.Template("Report", "0 30 6 * * *", false, List.of("Fetch", "Send"), List.of("recipients"),
                List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60"),
                    new WorkflowBundle.Parameter("limit", WorkflowParameterType.INT, "10"),
                    new WorkflowBundle.Parameter("region", WorkflowParameterType.STR, "us")))));
        final var review = new WorkflowImportReviewer(bundle, here).review();
        // the region is kept as it is here
        final var request = new WorkflowImportRequest(Map.of(WorkflowImportItem.keyOf(PARAMETER, "region", "Report"), Action.SKIP), Map.of(), true);

        // when
        final var written = workflowImportWriter.write(review, request);

        // then the definition which is here got the script of the file
        assertEquals(List.of(fetch), written.getDefinitions());
        assertSame(newScript, fetch.getScript());
        // the template which is here runs what the file says, in its order, with the cron of the file
        assertEquals(List.of(report), written.getTemplates());
        assertEquals("0 30 6 * * *", report.getCron());
        assertEquals(List.of(fetch, send), report.getWorkflowDefinitions());
        // and keeps its activation, as well as the parameter of its own which was not overwritten
        assertTrue(report.isEnabled());
        assertEquals(List.of("recipients", "timeout.8", "limit.8", "region.8"),
            report.getWorkflowParameters().stream().map(WorkflowParameter::getName).toList());
        assertSame(ownRegion, report.getWorkflowParameters().getLast());
        assertEquals(List.of("timeout.8=60", "limit.8=10"),
            written.getParameters().stream().map(parameter -> parameter.getName() + "=" + parameter.getValue()).toList());

        // a placeholder which is the same - the file has no value for it - is left alone
        assertEquals(List.of("SMTP.HOST"), written.getPlaceholders().stream().map(WorkflowPlaceholder::getName).toList());
        assertEquals("smtp.prod.local", written.getPlaceholders().getFirst().getValue());
        assertEquals("8", written.getTargets().get("TEMPLATE:Report"));
        assertEquals("limit.8", written.getTargets().get("PARAMETER:Report/limit"));
        assertFalse(written.getTargets().containsKey("PARAMETER:Report/region"));
    }

    @Test
    public void whenTemplateIsKeptThenOnlyWhatIsHereOfItsOwnParametersIsOverwritten() {
        // given
        final var ownTimeout = parameter("timeout.8", WorkflowParameterType.INT, "30");
        final var report = template(8, "Report", "0 0 6 * * *", true, List.of(), List.of(ownTimeout));
        final var here = content(List.of(report), List.of(), List.of(ownTimeout), List.of());
        final var bundle = new WorkflowBundle(1, null, List.of(), List.of(), List.of(),
            List.of(new WorkflowBundle.Template("Report", "0 30 6 * * *", true, List.of(), List.of(),
                List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60"),
                    new WorkflowBundle.Parameter("limit", WorkflowParameterType.INT, "10")))));
        final var review = new WorkflowImportReviewer(bundle, here).review();
        final var request = new WorkflowImportRequest(Map.of(WorkflowImportItem.keyOf(TEMPLATE, "Report", null), Action.SKIP), Map.of(), true);

        // when
        final var written = workflowImportWriter.write(review, request);

        // then the template is not saved, the parameter which is here is, and the new one has nowhere to be
        assertTrue(written.getTemplates().isEmpty());
        verify(workflowTemplateRepository, never()).save(any(WorkflowTemplate.class));
        assertEquals("0 0 6 * * *", report.getCron());
        assertEquals(List.of("timeout.8=60"), written.getParameters().stream().map(parameter -> parameter.getName() + "=" + parameter.getValue()).toList());
        assertEquals(Map.of("PARAMETER:Report/timeout", "timeout.8"), written.getTargets());
    }

    @Test
    public void whenValueIsLeftOutThenWhatIsHereKeepsItsValueWhateverElseChanges() {
        // given a parameter which is a password here and a text in the file, without a value
        final var here = content(List.of(), List.of(), List.of(parameter("token", WorkflowParameterType.PASS, "kept")), List.of());
        final var bundle = new WorkflowBundle(1, null, List.of(), List.of(new WorkflowBundle.Parameter("token", WorkflowParameterType.STR, null)), List.of(),
            List.of());
        final var review = new WorkflowImportReviewer(bundle, here).review();

        // when
        final var written = workflowImportWriter.write(review, new WorkflowImportRequest(Map.of(), Map.of("PARAMETER:token", "ignored"), true));

        // then
        final var token = written.getParameters().getFirst();
        assertEquals(WorkflowParameterType.STR, token.getType());
        assertEquals("kept", token.getValue());
    }

    @Test
    public void whenItemIsSkippedThenItIsNotSaved() {
        // given
        final var bundle = new WorkflowBundle(1, null, List.of(new WorkflowBundle.Placeholder("API.URL", "https://api")),
            List.of(new WorkflowBundle.Parameter("recipients", WorkflowParameterType.STR, "somebody")),
            List.of(new WorkflowBundle.Definition("Fetch", script(SLEEP_SCRIPT))),
            List.of(new WorkflowBundle.Template("Report", null, false, List.of(), List.of(),
                List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60")))));
        final var review = new WorkflowImportReviewer(bundle, NOTHING).review();
        final var request = new WorkflowImportRequest(Map.of(WorkflowImportItem.keyOf(PLACEHOLDER, "API.URL", null), Action.SKIP,
            WorkflowImportItem.keyOf(PARAMETER, "recipients", null), Action.SKIP, WorkflowImportItem.keyOf(DEFINITION, "Fetch", null), Action.SKIP,
            WorkflowImportItem.keyOf(TEMPLATE, "Report", null), Action.SKIP), Map.of(), true);

        // when
        final var written = workflowImportWriter.write(review, request);

        // then nothing is saved, the parameter of the skipped template included
        assertTrue(written.getTargets().isEmpty());
        verify(workflowPlaceholderRepository, never()).save(any(WorkflowPlaceholder.class));
        verify(workflowParameterRepository, never()).save(any(WorkflowParameter.class));
        verify(workflowDefinitionRepository, never()).save(any(WorkflowDefinition.class));
        verify(workflowTemplateRepository, never()).save(any(WorkflowTemplate.class));
    }

    @Test
    public void whenSomethingTheImportCountsOnIsGoneThenItGivesUp() {
        // given a definition which is here when the file is compared, and gone from the database when it is saved
        final var here = content(List.of(), List.of(definition(11, "Fetch", SLEEP_SCRIPT)), List.of(), List.of());
        final var bundle = new WorkflowBundle(1, null, List.of(new WorkflowBundle.Placeholder("API.URL", "https://api")), List.of(),
            List.of(new WorkflowBundle.Definition("Fetch", script(requestScript("https://api")))), List.of());
        final var review = new WorkflowImportReviewer(bundle, here).review();
        when(workflowDefinitionRepository.findById(11)).thenReturn(Optional.empty());

        // when and then the exception is what takes back the placeholder which was saved before, as it ends the transaction
        final var exception = assertThrows(WorkflowTransferException.class,
            () -> workflowImportWriter.write(review, new WorkflowImportRequest(Map.of(), Map.of(), true)));
        assertEquals(Reason.INSTANCE_CHANGED, exception.getReason());
    }

}
