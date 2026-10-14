package org.myworkflows.domain.transfer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.SLEEP_SCRIPT;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.content;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.definition;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.parameter;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.placeholder;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.requestScript;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.template;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowExportPlanTest {

    private static final int REPORT_ID = 3;
    private static final int CLEANUP_ID = 4;

    private WorkflowDefinition fetch;
    private WorkflowDefinition send;
    private WorkflowDefinition cleanup;

    private WorkflowParameter recipients;
    private WorkflowParameter smtpPassword;
    private WorkflowParameter timeout;
    private WorkflowParameter debug;

    private WorkflowPlaceholder apiUrl;
    private WorkflowPlaceholder apiPassword;
    private WorkflowPlaceholder unused;

    private WorkflowTemplate report;

    private WorkflowTransferContent content;

    @BeforeEach
    public void init() {
        fetch = definition(1, "Fetch", requestScript("$$(API.URL)/sales?key=$$(API.PASSWORD)"));
        send = definition(2, "Send", requestScript("$$(API.URL)/send and $$(GONE)"));
        cleanup = definition(5, "Cleanup", SLEEP_SCRIPT);
        recipients = parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com");
        smtpPassword = parameter("smtpPassword", WorkflowParameterType.PASS, "secret");
        timeout = parameter("timeout." + REPORT_ID, WorkflowParameterType.INT, "60");
        debug = parameter("debug", WorkflowParameterType.BOOL, "true");
        apiUrl = placeholder("API.URL", "https://api.example.com");
        apiPassword = placeholder("API.PASSWORD", "another secret");
        unused = placeholder("UNUSED", "nothing");
        report = template(REPORT_ID, "Report", "0 30 6 * * *", true, List.of(fetch, send), List.of(recipients, smtpPassword, timeout));
        final var cleanupTemplate = template(CLEANUP_ID, "Cleanup", null, false, List.of(cleanup), List.of(debug));
        content = content(List.of(report, cleanupTemplate), List.of(fetch, send, cleanup), List.of(recipients, smtpPassword, timeout, debug),
            List.of(apiUrl, apiPassword, unused));
    }

    @Test
    public void whenTemplateIsPickedThenWhatItNeedsComesAlong() {
        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(REPORT_ID), Set.of(), Set.of(), Set.of(), true, false), content);

        // then its definitions, the parameters it shares and the placeholders its definitions name are exported with it
        final var bundle = plan.toBundle(null);
        assertEquals(List.of("Report"), bundle.templates().stream().map(WorkflowBundle.Template::name).toList());
        assertEquals(List.of("Fetch", "Send"), bundle.definitions().stream().map(WorkflowBundle.Definition::name).toList());
        assertEquals(List.of("recipients", "smtpPassword"), bundle.parameters().stream().map(WorkflowBundle.Parameter::name).toList());
        // a placeholder which is named but does not exist is nothing to export
        assertEquals(List.of("API.PASSWORD", "API.URL"), bundle.placeholders().stream().map(WorkflowBundle.Placeholder::name).toList());

        // and each of them says what needs it, and cannot be left out
        assertEquals(new WorkflowExportPlan.Entry(true, true, List.of("Report"), null), plan.entryOf(fetch));
        assertEquals(new WorkflowExportPlan.Entry(true, true, List.of("Report"), null), plan.entryOf(recipients));
        assertEquals(new WorkflowExportPlan.Entry(true, true, List.of("Fetch", "Send"), null), plan.entryOf(apiUrl));
        assertEquals(new WorkflowExportPlan.Entry(true, false, List.of(), null), plan.entryOf(report));
        // while what nothing needs is left out, and can be picked
        assertEquals(new WorkflowExportPlan.Entry(false, false, List.of(), null), plan.entryOf(cleanup));
        assertEquals(new WorkflowExportPlan.Entry(false, false, List.of(), null), plan.entryOf(debug));
        assertEquals(new WorkflowExportPlan.Entry(false, false, List.of(), null), plan.entryOf(unused));

        assertEquals(1, plan.count(WorkflowTransferKind.TEMPLATE));
        assertEquals(2, plan.count(WorkflowTransferKind.DEFINITION));
        assertEquals(3, plan.count(WorkflowTransferKind.PARAMETER));
        assertEquals(2, plan.count(WorkflowTransferKind.PLACEHOLDER));
        assertEquals(8, plan.size());
        assertTrue(plan.isExportable());
    }

    @Test
    public void whenTemplateIsExportedThenItNamesWhatItRunsAndCarriesItsOwnParameters() {
        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(REPORT_ID), Set.of(), Set.of(), Set.of(), true, true), content);

        // then
        final var template = plan.toBundle(null).templates().getFirst();
        assertEquals("0 30 6 * * *", template.cron());
        assertTrue(template.enabled());
        assertEquals(List.of("Fetch", "Send"), template.definitions());
        assertEquals(List.of("recipients", "smtpPassword"), template.parameters());
        // the parameter which is meant for this template only is written under the name its workflow gets it by
        assertEquals(List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60")), template.ownParameters());
        // and it is no item of its own: it cannot be picked, and it follows its template
        assertEquals(new WorkflowExportPlan.Entry(true, true, List.of(), "Report"), plan.entryOf(timeout));
    }

    @Test
    public void whenParameterOfASingleTemplateIsPickedAloneThenItIsNotExported() {
        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(), Set.of(), Set.of(timeout.getName(), "debug"), Set.of(), true, true),
            content);

        // then
        assertEquals(List.of("debug"), plan.toBundle(null).parameters().stream().map(WorkflowBundle.Parameter::name).toList());
        assertEquals(new WorkflowExportPlan.Entry(false, true, List.of(), "Report"), plan.entryOf(timeout));
    }

    @Test
    public void whenDependenciesAreNotAskedForThenOnlyThePicksAreExported() {
        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(REPORT_ID), Set.of(5), Set.of(), Set.of("UNUSED"), false, true), content);

        // then
        final var bundle = plan.toBundle(null);
        assertEquals(List.of("Cleanup"), bundle.definitions().stream().map(WorkflowBundle.Definition::name).toList());
        assertTrue(bundle.parameters().isEmpty());
        assertEquals(List.of("UNUSED"), bundle.placeholders().stream().map(WorkflowBundle.Placeholder::name).toList());
        // the template still names what it runs, for the instance it is imported on to have it
        assertEquals(List.of("Fetch", "Send"), bundle.templates().getFirst().definitions());
        assertEquals(List.of("recipients", "smtpPassword"), bundle.templates().getFirst().parameters());
        // and its own parameter still travels with it, as it is a part of it
        assertEquals(1, bundle.templates().getFirst().ownParameters().size());
        assertEquals(new WorkflowExportPlan.Entry(false, false, List.of(), null), plan.entryOf(fetch));
    }

    @Test
    public void whenSecretsAreLeftOutThenTheirValuesAreNotWritten() {
        // given a parameter which is a secret by its name alone, next to the ones which are by their type
        final var dbPassword = parameter("dbPassword." + REPORT_ID, WorkflowParameterType.STR, "by its name");
        report.getWorkflowParameters().add(dbPassword);
        content = content(List.of(report), List.of(fetch, send), List.of(recipients, smtpPassword, timeout, dbPassword), List.of(apiUrl, apiPassword));

        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(REPORT_ID), Set.of(), Set.of(), Set.of(), true, false), content);

        // then
        final var bundle = plan.toBundle(null);
        assertEquals("ops@example.com", findParameter(bundle, "recipients").value());
        assertNull(findParameter(bundle, "smtpPassword").value());
        assertEquals("https://api.example.com", bundle.placeholders().get(1).value());
        assertNull(bundle.placeholders().getFirst().value());
        final var ownParameters = bundle.templates().getFirst().ownParameters();
        assertEquals(List.of(new WorkflowBundle.Parameter("dbPassword", WorkflowParameterType.STR, null),
            new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60")), ownParameters);
        assertEquals(3, plan.countLeftOutValues());
        assertTrue(plan.leavesOutValueOf(smtpPassword));
        assertTrue(plan.leavesOutValueOf(apiPassword));
        assertFalse(plan.leavesOutValueOf(recipients));
        assertFalse(plan.leavesOutValueOf(apiUrl));
    }

    @Test
    public void whenSecretsAreAskedForThenTheirValuesAreWritten() {
        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(REPORT_ID), Set.of(), Set.of(), Set.of(), true, true), content);

        // then
        final var bundle = plan.toBundle(null);
        assertEquals("secret", findParameter(bundle, "smtpPassword").value());
        assertEquals("another secret", bundle.placeholders().getFirst().value());
        assertEquals(0, plan.countLeftOutValues());
        assertFalse(plan.leavesOutValueOf(smtpPassword));
    }

    @Test
    public void whenTwoExportedItemsGoByTheSameNameThenTheExportIsRefused() {
        // given a second definition and a second template which take a name which is taken
        final var anotherFetch = definition(6, "Fetch", SLEEP_SCRIPT);
        final var anotherReport = template(7, "Report", null, false, List.of(), List.of());
        content = content(List.of(report, anotherReport), List.of(fetch, send, anotherFetch), List.of(recipients, smtpPassword, timeout),
            List.of(apiUrl, apiPassword));

        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(REPORT_ID, 7), Set.of(6), Set.of(), Set.of(), true, true), content);

        // then
        assertEquals(List.of("Report"), plan.findDuplicateNames(WorkflowTransferKind.TEMPLATE));
        assertEquals(List.of("Fetch"), plan.findDuplicateNames(WorkflowTransferKind.DEFINITION));
        assertFalse(plan.isExportable());
        // while one of each can be exported, as nothing in the file goes by the name of the other
        final var planOfOne = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(REPORT_ID), Set.of(), Set.of(), Set.of(), true, true), content);
        assertTrue(planOfOne.isExportable());
    }

    @Test
    public void whenNothingIsPickedThenThereIsNothingToExport() {
        // when
        final var plan = WorkflowExportPlan.of(new WorkflowExportSelection(Set.of(), Set.of(), Set.of(), Set.of(), true, false), content);

        // then
        assertEquals(0, plan.size());
        assertFalse(plan.isExportable());
    }

    private WorkflowBundle.Parameter findParameter(WorkflowBundle bundle, String name) {
        return bundle.parameters().stream().filter(parameter -> parameter.name().equals(name)).findFirst().orElseThrow();
    }

}
