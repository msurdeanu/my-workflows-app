package org.myworkflows.domain.transfer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportItem.State;
import org.myworkflows.domain.transfer.WorkflowImportNote.Type;

import java.util.List;
import java.util.Map;

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
public final class WorkflowImportReviewerTest {

    private static final WorkflowTransferContent NOTHING = content(List.of(), List.of(), List.of(), List.of());

    private static final WorkflowImportRequest IMPORT_ALL = new WorkflowImportRequest(Map.of(), Map.of(), true);

    @Test
    public void whenNothingIsHereThenEveryItemIsNew() {
        // given
        final var bundle = new WorkflowBundle(1, null,
            List.of(new WorkflowBundle.Placeholder("API.URL", "https://api"), new WorkflowBundle.Placeholder("API.PASSWORD", null)),
            List.of(new WorkflowBundle.Parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com"),
                new WorkflowBundle.Parameter("smtpPassword", WorkflowParameterType.PASS, null)),
            List.of(new WorkflowBundle.Definition("Fetch", script(SLEEP_SCRIPT))),
            List.of(new WorkflowBundle.Template("Report", "0 30 6 * * *", true, List.of("Fetch"), List.of("recipients", "smtpPassword"),
                List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60")))));

        // when
        final var review = new WorkflowImportReviewer(bundle, NOTHING).review();

        // then the items come kind by kind, a parameter of a single template among the parameters
        assertTrue(review.problems().isEmpty());
        assertEquals(List.of("TEMPLATE:Report", "DEFINITION:Fetch", "PARAMETER:recipients", "PARAMETER:smtpPassword", "PARAMETER:Report/timeout",
            "PLACEHOLDER:API.URL", "PLACEHOLDER:API.PASSWORD"), review.items().stream().map(WorkflowImportItem::key).toList());
        assertTrue(review.items().stream().allMatch(item -> item.state() == State.NEW && item.target() == null));
        assertEquals(7, review.count(State.NEW));

        // a template says what it comes with
        assertEquals(List.of(new WorkflowImportNote(Type.CONTENT, List.of("1", "3"))), find(review, TEMPLATE, "Report", null).notes());
        // a parameter says its type, and that it has no value when the file left it out
        assertEquals(List.of(WorkflowImportNote.of(Type.TYPE, "e")), find(review, PARAMETER, "recipients", null).notes());
        assertEquals(List.of(WorkflowImportNote.of(Type.TYPE, "p"), WorkflowImportNote.of(Type.VALUE_MISSING)),
            find(review, PARAMETER, "smtpPassword", null).notes());
        assertTrue(find(review, PARAMETER, "smtpPassword", null).secret());
        assertFalse(find(review, PARAMETER, "recipients", null).secret());
        assertEquals("Report", find(review, PARAMETER, "timeout", "Report").template());
        assertEquals(List.of(WorkflowImportNote.of(Type.VALUE_MISSING)), find(review, PLACEHOLDER, "API.PASSWORD", null).notes());
        assertTrue(find(review, PLACEHOLDER, "API.URL", null).notes().isEmpty());
        // what lacks a value needs a look, but can be imported
        assertEquals(2, review.countNeedingLook());
        assertTrue(WorkflowImportReviewer.check(review, IMPORT_ALL).isEmpty());
    }

    @Test
    public void whenItemIsHereThenItIsTheSameOrDifferent() {
        // given an instance which has everything the file has, half of it as the file has it
        final var fetch = definition(11, "Fetch", SLEEP_SCRIPT);
        final var send = definition(12, "Send", SLEEP_SCRIPT);
        final var recipients = parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com");
        final var retries = parameter("retries", WorkflowParameterType.STR, "3");
        final var here = content(List.of(), List.of(fetch, send), List.of(recipients, retries),
            List.of(placeholder("API.URL", "https://api"), placeholder("SMTP.HOST", "smtp.test.local")));
        final var bundle = new WorkflowBundle(1, null,
            List.of(new WorkflowBundle.Placeholder("API.URL", "https://api"), new WorkflowBundle.Placeholder("SMTP.HOST", "smtp.prod.local")),
            List.of(new WorkflowBundle.Parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com"),
                new WorkflowBundle.Parameter("retries", WorkflowParameterType.INT, "5")),
            List.of(new WorkflowBundle.Definition("Fetch", script(SLEEP_SCRIPT)), new WorkflowBundle.Definition("Send", script(requestScript("https://mail")))),
            List.of());

        // when
        final var review = new WorkflowImportReviewer(bundle, here).review();

        // then
        assertEquals(State.SAME, find(review, DEFINITION, "Fetch", null).state());
        assertEquals("11", find(review, DEFINITION, "Fetch", null).target());
        assertTrue(find(review, DEFINITION, "Fetch", null).notes().isEmpty());
        assertEquals(State.DIFFERENT, find(review, DEFINITION, "Send", null).state());
        assertEquals(List.of(WorkflowImportNote.of(Type.SCRIPT)), find(review, DEFINITION, "Send", null).notes());

        assertEquals(State.SAME, find(review, PARAMETER, "recipients", null).state());
        assertEquals(State.DIFFERENT, find(review, PARAMETER, "retries", null).state());
        assertEquals("retries", find(review, PARAMETER, "retries", null).target());
        assertEquals(List.of(WorkflowImportNote.of(Type.TYPE_CHANGE, "s", "i"), WorkflowImportNote.of(Type.VALUE, "3", "5")),
            find(review, PARAMETER, "retries", null).notes());

        assertEquals(State.SAME, find(review, PLACEHOLDER, "API.URL", null).state());
        assertEquals(State.DIFFERENT, find(review, PLACEHOLDER, "SMTP.HOST", null).state());
        assertEquals(List.of(WorkflowImportNote.of(Type.VALUE, "smtp.test.local", "smtp.prod.local")), find(review, PLACEHOLDER, "SMTP.HOST", null).notes());
        assertEquals(3, review.count(State.SAME));
        assertEquals(3, review.count(State.DIFFERENT));
        assertEquals(0, review.countNeedingLook());
    }

    @Test
    public void whenSecretIsHereThenItsValueIsNeitherShownNorReplacedByNothing() {
        // given
        final var here = content(List.of(), List.of(), List.of(parameter("smtpPassword", WorkflowParameterType.PASS, "mine"),
                parameter("apiPassword", WorkflowParameterType.STR, "mine too"), parameter("token", WorkflowParameterType.PASS, "kept")),
            List.of(placeholder("DB.PASSWORD", "mine as well")));
        final var bundle = new WorkflowBundle(1, null, List.of(new WorkflowBundle.Placeholder("DB.PASSWORD", null)),
            List.of(new WorkflowBundle.Parameter("smtpPassword", WorkflowParameterType.PASS, "theirs"),
                new WorkflowBundle.Parameter("apiPassword", WorkflowParameterType.STR, "theirs too"),
                new WorkflowBundle.Parameter("token", WorkflowParameterType.STR, null)),
            List.of(), List.of());

        // when
        final var review = new WorkflowImportReviewer(bundle, here).review();

        // then a secret which differs says so without its values, whether it is one by its type or by its name
        assertEquals(List.of(WorkflowImportNote.of(Type.SECRET_VALUE)), find(review, PARAMETER, "smtpPassword", null).notes());
        assertEquals(List.of(WorkflowImportNote.of(Type.SECRET_VALUE)), find(review, PARAMETER, "apiPassword", null).notes());
        assertEquals(State.DIFFERENT, find(review, PARAMETER, "apiPassword", null).state());
        // a value the file left out is no difference: the one here stays
        assertEquals(State.SAME, find(review, PLACEHOLDER, "DB.PASSWORD", null).state());
        assertEquals(List.of(WorkflowImportNote.of(Type.VALUE_KEPT)), find(review, PLACEHOLDER, "DB.PASSWORD", null).notes());
        // unless something else differs, which is then all that changes
        assertEquals(State.DIFFERENT, find(review, PARAMETER, "token", null).state());
        assertEquals(List.of(WorkflowImportNote.of(Type.TYPE_CHANGE, "p", "s"), WorkflowImportNote.of(Type.VALUE_KEPT)),
            find(review, PARAMETER, "token", null).notes());
        // and what is a secret here is one, whatever the file calls it
        assertTrue(find(review, PARAMETER, "token", null).secret());
    }

    @Test
    public void whenTemplateIsHereThenItIsComparedWithoutItsActivation() {
        // given a template which is paused here and has another cron, another order and another parameter
        final var fetch = definition(11, "Fetch", SLEEP_SCRIPT);
        final var send = definition(12, "Send", SLEEP_SCRIPT);
        final var recipients = parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com");
        final var ownTimeout = parameter("timeout.8", WorkflowParameterType.INT, "30");
        final var report = template(8, "Report", "0 0 6 * * *", false, List.of(send, fetch), List.of(recipients, ownTimeout));
        final var same = template(9, "Same", null, false, List.of(fetch), List.of(recipients));
        final var here = content(List.of(report, same), List.of(fetch, send), List.of(recipients, ownTimeout), List.of());
        final var bundle = new WorkflowBundle(1, null, List.of(), List.of(), List.of(),
            List.of(new WorkflowBundle.Template("Report", "0 30 6 * * *", true, List.of("Fetch", "Send"), List.of("recipients", "debug"),
                    List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60"),
                        new WorkflowBundle.Parameter("limit", WorkflowParameterType.INT, "10"))),
                new WorkflowBundle.Template("Same", " ", true, List.of("Fetch"), List.of("recipients"), List.of())));

        // when
        final var review = new WorkflowImportReviewer(bundle, here).review();

        // then
        final var reportItem = find(review, TEMPLATE, "Report", null);
        assertEquals(State.DIFFERENT, reportItem.state());
        assertEquals("8", reportItem.target());
        assertEquals(List.of(WorkflowImportNote.of(Type.CRON, "0 0 6 * * *", "0 30 6 * * *"), WorkflowImportNote.of(Type.DEFINITIONS, "Send, Fetch", "Fetch, Send"),
            WorkflowImportNote.of(Type.PARAMETERS, "recipients, timeout", "debug, limit, recipients, timeout")), reportItem.notes());
        // a template which is ticked in the file and not here is the same: what is here keeps its activation
        assertEquals(State.SAME, find(review, TEMPLATE, "Same", null).state());

        // the parameter of the template alone is found under the name it is saved by here, which ends with another id
        final var timeoutItem = find(review, PARAMETER, "timeout", "Report");
        assertEquals(State.DIFFERENT, timeoutItem.state());
        assertEquals("timeout.8", timeoutItem.target());
        assertEquals(List.of(WorkflowImportNote.of(Type.VALUE, "30", "60")), timeoutItem.notes());
        assertEquals(State.NEW, find(review, PARAMETER, "limit", "Report").state());

        // the template names a parameter which is neither in the file nor here, so it cannot be imported
        final var problems = WorkflowImportReviewer.check(review, IMPORT_ALL);
        assertEquals(List.of(new WorkflowImportProblem(WorkflowImportProblem.Type.PARAMETER_MISSING, TEMPLATE, "Report", "debug")), problems);
        // unless it is kept as it is here
        final var keepingReport = new WorkflowImportRequest(Map.of(reportItem.key(), Action.SKIP), Map.of(), true);
        assertTrue(WorkflowImportReviewer.check(review, keepingReport).isEmpty());
        // and then its new parameter stays out too, while the one which is here can still be overwritten
        assertEquals(Action.SKIP, review.actionOf(find(review, PARAMETER, "limit", "Report"), keepingReport));
        assertEquals(Action.IMPORT, review.actionOf(timeoutItem, keepingReport));
        assertEquals(Action.IMPORT, review.actionOf(find(review, PARAMETER, "limit", "Report"), IMPORT_ALL));
        assertEquals(Action.SKIP, review.actionOf(find(review, TEMPLATE, "Same", null), IMPORT_ALL));
    }

    @Test
    public void whenTemplateNamesWhatIsNotImportedThenItCannotBeImportedEither() {
        // given a file which brings a template with its definition
        final var bundle = new WorkflowBundle(1, null, List.of(), List.of(new WorkflowBundle.Parameter("recipients", WorkflowParameterType.STR, "somebody")),
            List.of(new WorkflowBundle.Definition("Fetch", script(SLEEP_SCRIPT))),
            List.of(new WorkflowBundle.Template("Report", null, false, List.of("Fetch"), List.of("recipients"), List.of())));
        final var review = new WorkflowImportReviewer(bundle, NOTHING).review();
        assertTrue(WorkflowImportReviewer.check(review, IMPORT_ALL).isEmpty());

        // when the definition and the parameter are skipped
        final var skipping = new WorkflowImportRequest(Map.of(WorkflowImportItem.keyOf(DEFINITION, "Fetch", null), Action.SKIP,
            WorkflowImportItem.keyOf(PARAMETER, "recipients", null), Action.SKIP), Map.of(), true);

        // then
        assertEquals(List.of(new WorkflowImportProblem(WorkflowImportProblem.Type.DEFINITION_MISSING, TEMPLATE, "Report", "Fetch"),
            new WorkflowImportProblem(WorkflowImportProblem.Type.PARAMETER_MISSING, TEMPLATE, "Report", "recipients")),
            WorkflowImportReviewer.check(review, skipping));
        // while skipping the template as well leaves nothing to complain about
        final var skippingAll = new WorkflowImportRequest(Map.of(WorkflowImportItem.keyOf(DEFINITION, "Fetch", null), Action.SKIP,
            WorkflowImportItem.keyOf(PARAMETER, "recipients", null), Action.SKIP, WorkflowImportItem.keyOf(TEMPLATE, "Report", null), Action.SKIP),
            Map.of(), true);
        assertTrue(WorkflowImportReviewer.check(review, skippingAll).isEmpty());
    }

    @Test
    public void whenItemCannotBeImportedAsItIsThenItHasToBeSkipped() {
        // given a parameter whose value does not go with its type, and a template whose cron is none
        final var bundle = new WorkflowBundle(1, null, List.of(),
            List.of(new WorkflowBundle.Parameter("retries", WorkflowParameterType.INT, "many"),
                new WorkflowBundle.Parameter("port", WorkflowParameterType.INT, null)),
            List.of(), List.of(new WorkflowBundle.Template("Report", "every day", false, List.of(), List.of(), List.of())));

        // when
        final var review = new WorkflowImportReviewer(bundle, NOTHING).review();

        // then
        final var retries = find(review, PARAMETER, "retries", null);
        assertTrue(retries.isInvalid());
        assertEquals(List.of(WorkflowImportNote.of(Type.TYPE, "i"), WorkflowImportNote.of(Type.VALUE_INVALID, "Value is not an int")), retries.notes());
        final var report = find(review, TEMPLATE, "Report", null);
        assertTrue(report.has(Type.CRON_INVALID));
        assertEquals(List.of(WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_INVALID, TEMPLATE, "Report"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_INVALID, PARAMETER, "retries")), WorkflowImportReviewer.check(review, IMPORT_ALL));

        // a value which is typed for what the file left out has to go with its type too
        final var skipping = Map.of(retries.key(), Action.SKIP, report.key(), Action.SKIP);
        final var port = find(review, PARAMETER, "port", null);
        assertEquals(List.of(new WorkflowImportProblem(WorkflowImportProblem.Type.VALUE_INVALID, PARAMETER, "port", "Value is not an int")),
            WorkflowImportReviewer.check(review, new WorkflowImportRequest(skipping, Map.of(port.key(), "eighty"), true)));
        assertTrue(WorkflowImportReviewer.check(review, new WorkflowImportRequest(skipping, Map.of(port.key(), "80"), true)).isEmpty());
        assertTrue(WorkflowImportReviewer.check(review, new WorkflowImportRequest(skipping, Map.of(), true)).isEmpty());
    }

    @Test
    public void whenDefinitionUsesCommandsWhichAreNotKnownThenItSaysWhich() {
        // given
        final var bundle = new WorkflowBundle(1, null, List.of(), List.of(), List.of(new WorkflowBundle.Definition("Archive", script("""
            commands:
            - class: s3Upload
              name: Upload
            - class: goneLoop
              name: Loop
              subcommands:
              - class: zip
                name: Inside
            finallyCommands:
            - class: s3Upload
              name: Upload again
            """))), List.of());

        // when
        final var review = new WorkflowImportReviewer(bundle, NOTHING).review();

        // then the definition can be imported, and needs a look
        final var archive = find(review, DEFINITION, "Archive", null);
        assertEquals(List.of(WorkflowImportNote.of(Type.UNKNOWN_COMMANDS, "goneLoop, s3Upload, zip")), archive.notes());
        assertTrue(archive.needsLook());
        assertFalse(archive.isInvalid());
        assertTrue(WorkflowImportReviewer.check(review, IMPORT_ALL).isEmpty());
    }

    @Test
    public void whenFileCannotBeMatchedThenNothingOfItIsReviewed() {
        // given two definitions and two templates which go by the same name here
        final var here = content(List.of(template(1, "Twice", null, false, List.of(), List.of()), template(2, "Twice", null, false, List.of(), List.of())),
            List.of(definition(3, "Double", SLEEP_SCRIPT), definition(4, "Double", SLEEP_SCRIPT), definition(5, "Named", SLEEP_SCRIPT),
                definition(6, "Named", SLEEP_SCRIPT)), List.of(), List.of());
        final var bundle = new WorkflowBundle(1, null,
            List.of(new WorkflowBundle.Placeholder("lower", "x"), new WorkflowBundle.Placeholder("SAME", "x"), new WorkflowBundle.Placeholder("SAME", "y")),
            List.of(new WorkflowBundle.Parameter("with space", WorkflowParameterType.STR, "x"), new WorkflowBundle.Parameter("untyped", null, "x")),
            List.of(new WorkflowBundle.Definition("Double", script(SLEEP_SCRIPT)), new WorkflowBundle.Definition(" ", script(SLEEP_SCRIPT)),
                new WorkflowBundle.Definition("Empty", null)),
            List.of(new WorkflowBundle.Template("Twice", null, false, List.of("Named"), List.of(), List.of()),
                new WorkflowBundle.Template("Once", null, false, List.of(), List.of(), List.of()),
                new WorkflowBundle.Template("Once", null, false, List.of(), List.of(), List.of())));

        // when
        final var review = new WorkflowImportReviewer(bundle, here).review();

        // then
        assertTrue(review.items().isEmpty());
        assertEquals(List.of(
            WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_UNUSABLE, DEFINITION, " "),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_UNUSABLE, DEFINITION, "Empty"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_UNUSABLE, PARAMETER, "with space"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_UNUSABLE, PARAMETER, "untyped"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_UNUSABLE, PLACEHOLDER, "lower"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.DUPLICATE_IN_FILE, TEMPLATE, "Once"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.DUPLICATE_IN_FILE, PLACEHOLDER, "SAME"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.AMBIGUOUS_HERE, TEMPLATE, "Twice"),
            WorkflowImportProblem.of(WorkflowImportProblem.Type.AMBIGUOUS_HERE, DEFINITION, "Double"),
            // a definition a template of the file names is looked for here too, though the file does not bring it
            WorkflowImportProblem.of(WorkflowImportProblem.Type.AMBIGUOUS_HERE, DEFINITION, "Named")), review.problems());
    }

    @Test
    public void whenInstanceChangesThenTheReviewIsNoLongerTheOneWhichWasShown() {
        // given
        final var bundle = new WorkflowBundle(1, null, List.of(new WorkflowBundle.Placeholder("API.URL", "https://api")), List.of(), List.of(), List.of());
        final var shown = new WorkflowImportReviewer(bundle, NOTHING).review();

        // when and then
        assertTrue(new WorkflowImportReviewer(bundle, NOTHING).review().hasStatesOf(shown));
        assertFalse(new WorkflowImportReviewer(bundle, content(List.of(), List.of(), List.of(), List.of(placeholder("API.URL", "https://api"))))
            .review().hasStatesOf(shown));
        assertNull(shown.items().getFirst().target());
        assertEquals("API.URL", bundle.placeholders().getFirst().name());
    }

    private WorkflowImportItem find(WorkflowImportReview review, WorkflowTransferKind kind, String name, String template) {
        return review.find(kind, name, template).orElseThrow();
    }

}
