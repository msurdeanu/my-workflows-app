package org.myworkflows.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.User;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.domain.transfer.WorkflowBundleFormat;
import org.myworkflows.domain.transfer.WorkflowExportSelection;
import org.myworkflows.domain.transfer.WorkflowImportItem;
import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportItem.State;
import org.myworkflows.domain.transfer.WorkflowImportProblem;
import org.myworkflows.domain.transfer.WorkflowImportRequest;
import org.myworkflows.domain.transfer.WorkflowImportResult.Change;
import org.myworkflows.domain.transfer.WorkflowImportResult.Outcome;
import org.myworkflows.domain.transfer.WorkflowImportResult.Todo;
import org.myworkflows.exception.WorkflowTransferException;
import org.myworkflows.exception.WorkflowTransferException.Reason;
import org.myworkflows.serializer.WorkflowBundleSerializer;
import org.myworkflows.service.WorkflowImportWriter.Written;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.info.BuildProperties;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.SLEEP_SCRIPT;
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
public final class WorkflowTransferServiceTest {

    private final List<WorkflowTemplate> templates = new ArrayList<>();
    private final List<WorkflowDefinition> definitions = new ArrayList<>();
    private final List<WorkflowParameter> parameters = new ArrayList<>();
    private final List<WorkflowPlaceholder> placeholders = new ArrayList<>();

    @Mock
    private WorkflowTemplateService workflowTemplateService;

    @Mock
    private WorkflowDefinitionService workflowDefinitionService;

    @Mock
    private WorkflowParameterService workflowParameterService;

    @Mock
    private WorkflowPlaceholderService workflowPlaceholderService;

    @Mock
    private WorkflowImportWriter workflowImportWriter;

    @Mock
    private ObjectProvider<BuildProperties> buildProperties;

    private WorkflowTransferService workflowTransferService;

    @BeforeEach
    public void init() {
        workflowTransferService = new WorkflowTransferService(workflowTemplateService, workflowDefinitionService, workflowParameterService,
            workflowPlaceholderService, workflowImportWriter, buildProperties);
        // what the instance holds is read again each time somebody asks, like the caches are
        lenient().when(workflowTemplateService.getAllItems()).thenAnswer(_ -> templates.stream());
        lenient().when(workflowDefinitionService.getAllItems()).thenAnswer(_ -> definitions.stream());
        lenient().when(workflowParameterService.getAllItems()).thenAnswer(_ -> parameters.stream());
        lenient().when(workflowPlaceholderService.getAllItems()).thenAnswer(_ -> placeholders.stream());
    }

    @AfterEach
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }

    @Test
    public void whenSelectionIsExportedThenTheFileSaysWhatWasPickedAndWhereItComesFrom() {
        // given
        final var fetch = definition(1, "Fetch", requestScript("$$(API.URL)/sales"));
        definitions.add(fetch);
        parameters.add(parameter("smtpPassword", WorkflowParameterType.PASS, "secret"));
        placeholders.add(placeholder("API.URL", "https://api"));
        templates.add(template(3, "Report", "0 30 6 * * *", true, List.of(fetch), List.of(parameters.getFirst())));
        final var user = User.of("admin", "a hash");
        SecurityContextHolder.getContext().setAuthentication(new UsernamePasswordAuthenticationToken(new UserAccountDetails(user), null, List.of()));
        final var selection = new WorkflowExportSelection(Set.of(3), Set.of(), Set.of(), Set.of(), true, false);

        // when
        final var file = workflowTransferService.export(selection, WorkflowBundleFormat.YAML);

        // then the file is named after the day and holds the template with what it needs
        assertEquals("my-workflows-" + LocalDate.now() + ".yaml", file.name());
        assertEquals("application/yaml", file.contentType());
        final var bundle = WorkflowBundleSerializer.toBundle(file.content());
        assertEquals(WorkflowBundle.FORMAT, bundle.format());
        assertEquals("admin", bundle.exported().by());
        // the version is the one of the jar, which a build of the tests does not have
        assertEquals("dev", bundle.exported().version());
        assertNotNull(bundle.exported().at());
        assertEquals(List.of("Report"), bundle.templates().stream().map(WorkflowBundle.Template::name).toList());
        assertEquals(List.of("Fetch"), bundle.definitions().stream().map(WorkflowBundle.Definition::name).toList());
        assertEquals(List.of(new WorkflowBundle.Parameter("smtpPassword", WorkflowParameterType.PASS, null)), bundle.parameters());
        assertEquals(List.of(new WorkflowBundle.Placeholder("API.URL", "https://api")), bundle.placeholders());

        // and an archive is the same bundle in another file
        final var archive = workflowTransferService.export(selection, WorkflowBundleFormat.ZIP);
        assertEquals("my-workflows-" + LocalDate.now() + ".zip", archive.name());
        assertEquals("application/zip", archive.contentType());
        assertArrayEquals(new byte[] {'P', 'K'}, new byte[] {archive.content()[0], archive.content()[1]});
        assertEquals(bundle.templates(), WorkflowBundleSerializer.toBundle(archive.content()).templates());
    }

    @Test
    public void whenSelectionCannotBeExportedThenTheReasonSaysWhy() {
        // given two definitions which go by the same name
        definitions.add(definition(1, "Twice", SLEEP_SCRIPT));
        definitions.add(definition(2, "Twice", SLEEP_SCRIPT));

        // when and then
        final var nothing = assertThrows(WorkflowTransferException.class, () -> workflowTransferService.export(
            new WorkflowExportSelection(Set.of(), Set.of(), Set.of(), Set.of(), true, false), WorkflowBundleFormat.YAML));
        assertEquals(Reason.NOTHING_TO_EXPORT, nothing.getReason());
        final var duplicates = assertThrows(WorkflowTransferException.class, () -> workflowTransferService.export(
            new WorkflowExportSelection(Set.of(), Set.of(1, 2), Set.of(), Set.of(), true, false), WorkflowBundleFormat.YAML));
        assertEquals(Reason.DUPLICATE_NAMES, duplicates.getReason());
        assertArrayEquals(new Object[] {"Twice"}, duplicates.getArguments());
    }

    @Test
    public void whenImportIsAppliedThenWhatWasSavedIsTakenOverByTheCaches() {
        // given an instance which has a definition, a parameter, a placeholder and a template of the file
        final var fetch = definition(11, "Fetch", SLEEP_SCRIPT);
        final var retries = parameter("retries", WorkflowParameterType.STR, "3");
        final var smtpHost = placeholder("SMTP.HOST", "smtp.test.local");
        final var report = template(8, "Report", "0 0 6 * * *", true, List.of(fetch), List.of(retries));
        definitions.add(fetch);
        parameters.add(retries);
        placeholders.add(smtpHost);
        templates.add(report);
        final var review = workflowTransferService.read(("""
            format: 1
            placeholders:
            - name: SMTP.HOST
              value: smtp.prod.local
            - name: API.PASSWORD
            parameters:
            - name: retries
              type: INT
              value: 5
            definitions:
            - name: Fetch
              script:
                commands:
                - class: s3Upload
                  name: Upload
            - name: Send
              script:
            """ + SLEEP_SCRIPT.indent(4) + """
            templates:
            - name: Report
              cron: 0 30 6 * * *
              enabled: true
              definitions: [Fetch, Send]
              parameters: [retries]
            - name: Nightly
              cron: 0 0 1 * * *
              enabled: true
              definitions: [Send]
              parameters: []
            """).getBytes(UTF_8));
        assertEquals(4, review.count(State.DIFFERENT));
        assertEquals(3, review.count(State.NEW));
        final var request = new WorkflowImportRequest(Map.of(), Map.of(), true);
        assertEquals(List.of(), workflowTransferService.check(review, request));

        // and a writer which saved all of it
        final var savedFetch = definition(11, "Fetch", requestScript("https://saved"));
        final var savedSend = definition(12, "Send", SLEEP_SCRIPT);
        final var savedRetries = parameter("retries", WorkflowParameterType.INT, "5");
        final var savedSmtpHost = placeholder("SMTP.HOST", "smtp.prod.local");
        final var savedApiPassword = placeholder("API.PASSWORD", "");
        final var savedReport = template(8, "Report", "0 30 6 * * *", true, List.of(savedFetch, savedSend), List.of(savedRetries));
        final var savedNightly = template(9, "Nightly", "0 0 1 * * *", false, List.of(savedSend), List.of());
        final var written = new Written();
        written.getPlaceholders().addAll(List.of(savedSmtpHost, savedApiPassword));
        written.getParameters().add(savedRetries);
        written.getDefinitions().addAll(List.of(savedFetch, savedSend));
        written.getTemplates().addAll(List.of(savedReport, savedNightly));
        written.getTargets().putAll(Map.of("PLACEHOLDER:SMTP.HOST", "SMTP.HOST", "PLACEHOLDER:API.PASSWORD", "API.PASSWORD", "PARAMETER:retries", "retries",
            "DEFINITION:Fetch", "11", "DEFINITION:Send", "12", "TEMPLATE:Report", "8", "TEMPLATE:Nightly", "9"));
        when(workflowImportWriter.write(any(), any())).thenReturn(written);

        // when
        final var result = workflowTransferService.apply(review, request);

        // then what was here is the object it always was, with what was saved
        assertEquals("smtp.prod.local", smtpHost.getValue());
        assertEquals(WorkflowParameterType.INT, retries.getType());
        assertEquals("5", retries.getValue());
        assertSame(savedFetch.getScript(), fetch.getScript());
        // and the templates which hold a copy of it are told
        verify(workflowTemplateService).propagateInternalUpdate(retries);
        verify(workflowTemplateService).propagateInternalUpdate(fetch);
        verify(workflowTemplateService).applyInternalUpdate(report, savedReport);
        // while what is new is put into the caches as it was saved
        verify(workflowPlaceholderService).create(savedApiPassword, false);
        verify(workflowDefinitionService).create(savedSend, false);
        verify(workflowTemplateService).create(savedNightly, false);
        verify(workflowPlaceholderService, never()).create(savedSmtpHost, false);
        verify(workflowParameterService, never()).create(any(), anyBoolean());
        verify(workflowTemplateService, never()).create(savedReport, false);

        // every item is told what happened to it, and where it is by now
        assertEquals(List.of(new Outcome(TEMPLATE, "Nightly", null, "9", Change.CREATED), new Outcome(DEFINITION, "Send", null, "12", Change.CREATED),
            new Outcome(PLACEHOLDER, "API.PASSWORD", null, "API.PASSWORD", Change.CREATED)), result.findOutcomes(Change.CREATED));
        assertEquals(List.of(new Outcome(TEMPLATE, "Report", null, "8", Change.OVERWRITTEN), new Outcome(DEFINITION, "Fetch", null, "11", Change.OVERWRITTEN),
            new Outcome(PARAMETER, "retries", null, "retries", Change.OVERWRITTEN),
            new Outcome(PLACEHOLDER, "SMTP.HOST", null, "SMTP.HOST", Change.OVERWRITTEN)), result.findOutcomes(Change.OVERWRITTEN));
        assertEquals(7, result.countImported());
        // and what is left to do is listed: a template which arrived paused, a command nobody brings, a value nobody typed
        assertEquals(List.of(new Todo(Todo.Type.TEMPLATE_PAUSED, new Outcome(TEMPLATE, "Nightly", null, "9", Change.CREATED), null),
            new Todo(Todo.Type.UNKNOWN_COMMANDS, new Outcome(DEFINITION, "Fetch", null, "11", Change.OVERWRITTEN), "s3Upload"),
            new Todo(Todo.Type.VALUE_MISSING, new Outcome(PLACEHOLDER, "API.PASSWORD", null, "API.PASSWORD", Change.CREATED), null)), result.todos());
    }

    @Test
    public void whenItemsAreNotImportedThenTheyAreKeptOrSkipped() {
        // given
        placeholders.add(placeholder("SMTP.HOST", "smtp.test.local"));
        placeholders.add(placeholder("API.URL", "https://api"));
        final var review = workflowTransferService.read("""
            format: 1
            placeholders:
            - name: SMTP.HOST
              value: smtp.prod.local
            - name: API.URL
              value: https://api
            - name: API.PASSWORD
            - name: NEW.ONE
              value: new
            """.getBytes(UTF_8));
        final var request = new WorkflowImportRequest(Map.of(WorkflowImportItem.keyOf(PLACEHOLDER, "SMTP.HOST", null), Action.SKIP,
            WorkflowImportItem.keyOf(PLACEHOLDER, "NEW.ONE", null), Action.SKIP), Map.of("PLACEHOLDER:API.PASSWORD", "typed"), true);
        final var written = new Written();
        written.getPlaceholders().add(placeholder("API.PASSWORD", "typed"));
        written.getTargets().put("PLACEHOLDER:API.PASSWORD", "API.PASSWORD");
        when(workflowImportWriter.write(any(), any())).thenReturn(written);

        // when
        final var result = workflowTransferService.apply(review, request);

        // then what is here and was not imported is kept, and what is not here is nowhere
        assertEquals(List.of(new Outcome(PLACEHOLDER, "SMTP.HOST", null, "SMTP.HOST", Change.KEPT),
            new Outcome(PLACEHOLDER, "API.URL", null, "API.URL", Change.KEPT)), result.findOutcomes(Change.KEPT));
        assertEquals(List.of(new Outcome(PLACEHOLDER, "NEW.ONE", null, null, Change.SKIPPED)), result.findOutcomes(Change.SKIPPED));
        assertEquals(1, result.countImported());
        // a value which was typed leaves nothing to do
        assertTrue(result.todos().isEmpty());
    }

    @Test
    public void whenInstanceChangedSinceTheFileWasComparedThenNothingIsSaved() {
        // given a file which was compared with an instance which did not have its placeholder yet
        final var review = workflowTransferService.read("format: 1\nplaceholders:\n- name: API.URL\n  value: https://api\n".getBytes(UTF_8));
        assertEquals(State.NEW, review.items().getFirst().state());
        placeholders.add(placeholder("API.URL", "somebody was faster"));

        // when and then
        final var exception = assertThrows(WorkflowTransferException.class,
            () -> workflowTransferService.apply(review, new WorkflowImportRequest(Map.of(), Map.of(), true)));
        assertEquals(Reason.INSTANCE_CHANGED, exception.getReason());
        verify(workflowImportWriter, never()).write(any(), any());
        assertEquals("somebody was faster", placeholders.getFirst().getValue());
    }

    @Test
    public void whenChoicesCannotBeAppliedThenNothingIsSaved() {
        // given a template whose definition is skipped
        final var review = workflowTransferService.review(new WorkflowBundle(1, null, List.of(), List.of(),
            List.of(new WorkflowBundle.Definition("Fetch", script(SLEEP_SCRIPT))),
            List.of(new WorkflowBundle.Template("Report", null, false, List.of("Fetch"), List.of(), List.of()))));
        final var request = new WorkflowImportRequest(Map.of(WorkflowImportItem.keyOf(DEFINITION, "Fetch", null), Action.SKIP), Map.of(), true);
        assertEquals(List.of(new WorkflowImportProblem(WorkflowImportProblem.Type.DEFINITION_MISSING, TEMPLATE, "Report", "Fetch")),
            workflowTransferService.check(review, request));

        // when and then
        final var exception = assertThrows(WorkflowTransferException.class, () -> workflowTransferService.apply(review, request));
        assertEquals(Reason.NOT_IMPORTABLE, exception.getReason());
        verify(workflowImportWriter, never()).write(any(), any());
    }

    @Test
    public void whenFileHasProblemsThenNothingOfItIsSaved() {
        // given a file with two placeholders of the same name
        final var review = workflowTransferService.read("format: 1\nplaceholders:\n- name: SAME\n  value: a\n- name: SAME\n  value: b\n".getBytes(UTF_8));
        assertEquals(List.of(WorkflowImportProblem.of(WorkflowImportProblem.Type.DUPLICATE_IN_FILE, PLACEHOLDER, "SAME")), review.problems());

        // when and then
        final var exception = assertThrows(WorkflowTransferException.class,
            () -> workflowTransferService.apply(review, new WorkflowImportRequest(Map.of(), Map.of(), true)));
        assertEquals(Reason.NOT_IMPORTABLE, exception.getReason());
        verify(workflowImportWriter, never()).write(any(), any());
    }

}
