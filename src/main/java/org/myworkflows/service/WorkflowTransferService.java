package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.domain.transfer.WorkflowBundleFormat;
import org.myworkflows.domain.transfer.WorkflowExportFile;
import org.myworkflows.domain.transfer.WorkflowExportPlan;
import org.myworkflows.domain.transfer.WorkflowExportSelection;
import org.myworkflows.domain.transfer.WorkflowImportItem;
import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportItem.State;
import org.myworkflows.domain.transfer.WorkflowImportNote;
import org.myworkflows.domain.transfer.WorkflowImportProblem;
import org.myworkflows.domain.transfer.WorkflowImportRequest;
import org.myworkflows.domain.transfer.WorkflowImportResult;
import org.myworkflows.domain.transfer.WorkflowImportResult.Change;
import org.myworkflows.domain.transfer.WorkflowImportResult.Outcome;
import org.myworkflows.domain.transfer.WorkflowImportResult.Todo;
import org.myworkflows.domain.transfer.WorkflowImportReview;
import org.myworkflows.domain.transfer.WorkflowImportReviewer;
import org.myworkflows.domain.transfer.WorkflowTransferContent;
import org.myworkflows.domain.transfer.WorkflowTransferKind;
import org.myworkflows.exception.WorkflowTransferException;
import org.myworkflows.exception.WorkflowTransferException.Reason;
import org.myworkflows.serializer.WorkflowBundleSerializer;
import org.myworkflows.service.WorkflowImportWriter.Written;
import org.myworkflows.util.SecurityUtil;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.info.BuildProperties;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;

/**
 * Moves templates, definitions, parameters and placeholders from one instance to another as a file: an export writes
 * what was picked, an import compares a file with what is here, lets somebody decide item by item and then saves what
 * was decided, all of it or nothing.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Service
@RequiredArgsConstructor
public final class WorkflowTransferService {

    private static final String FILE_NAME_PREFIX = "my-workflows-";

    private static final String UNKNOWN_VERSION = "dev";

    private static final String NAME_SEPARATOR = ", ";

    // one import at a time: each of them counts on what is here to stay as it found it
    private final Lock importLock = new ReentrantLock();

    private final WorkflowTemplateService workflowTemplateService;

    private final WorkflowDefinitionService workflowDefinitionService;

    private final WorkflowParameterService workflowParameterService;

    private final WorkflowPlaceholderService workflowPlaceholderService;

    private final WorkflowImportWriter workflowImportWriter;

    private final ObjectProvider<BuildProperties> buildProperties;

    /**
     * @return what the instance holds by now of everything an export can write.
     */
    public WorkflowTransferContent findContent() {
        return new WorkflowTransferContent(workflowTemplateService.getAllItems().toList(), workflowDefinitionService.getAllItems().toList(),
            workflowParameterService.getAllItems().toList(), workflowPlaceholderService.getAllItems().toList());
    }

    /**
     * Has to be called on a thread serving the user, whose name is written to the file.
     *
     * @return the file of the given selection, named after the day it is written on.
     */
    public WorkflowExportFile export(WorkflowExportSelection selection, WorkflowBundleFormat format) {
        final var plan = WorkflowExportPlan.of(selection, findContent());
        if (plan.size() == 0) {
            throw new WorkflowTransferException(Reason.NOTHING_TO_EXPORT);
        }
        final var duplicateNames = Stream.of(WorkflowTransferKind.TEMPLATE, WorkflowTransferKind.DEFINITION)
            .flatMap(kind -> plan.findDuplicateNames(kind).stream())
            .toList();
        if (!duplicateNames.isEmpty()) {
            throw new WorkflowTransferException(Reason.DUPLICATE_NAMES, String.join(NAME_SEPARATOR, duplicateNames));
        }

        final var now = Instant.now();
        final var origin = new WorkflowBundle.Origin(now.truncatedTo(ChronoUnit.SECONDS),
            SecurityUtil.findCurrentAccount().map(UserAccountDetails::getUsername).orElse(null),
            ofNullable(buildProperties.getIfAvailable()).map(BuildProperties::getVersion).orElse(UNKNOWN_VERSION));
        return new WorkflowExportFile(createFileName(format), format.getContentType(), WorkflowBundleSerializer.toBytes(plan.toBundle(origin), format));
    }

    /**
     * @return the name of a file which is exported now: it says the day, where the application runs.
     */
    public String createFileName(WorkflowBundleFormat format) {
        return FILE_NAME_PREFIX + LocalDate.now(ZoneId.systemDefault()) + format.getExtension();
    }

    /**
     * @param content a file an export wrote, on this instance or on another one.
     * @return what the file holds, compared with what is here. Nothing is changed yet.
     */
    public WorkflowImportReview read(byte[] content) {
        return review(WorkflowBundleSerializer.toBundle(content));
    }

    public WorkflowImportReview review(WorkflowBundle bundle) {
        return new WorkflowImportReviewer(bundle, findContent()).review();
    }

    /**
     * @return what keeps the given choices from being applied, nothing when they can be.
     */
    public List<WorkflowImportProblem> check(WorkflowImportReview review, WorkflowImportRequest request) {
        return WorkflowImportReviewer.check(review, request);
    }

    /**
     * Saves what was chosen for the items of a file. The file is compared once more with what is here, as somebody may
     * have changed it since the choices were made: when an item is no longer in the state it was shown in, nothing is
     * saved and the choices have to be made again.
     *
     * @param review the file as it was shown to whoever made the choices.
     * @return what was done with every item of the file.
     */
    public WorkflowImportResult apply(WorkflowImportReview review, WorkflowImportRequest request) {
        importLock.lock();
        try {
            final var currentReview = review(review.bundle());
            if (!currentReview.hasStatesOf(review)) {
                throw new WorkflowTransferException(Reason.INSTANCE_CHANGED);
            }
            if (!currentReview.problems().isEmpty() || !check(currentReview, request).isEmpty()) {
                throw new WorkflowTransferException(Reason.NOT_IMPORTABLE);
            }

            final var written = workflowImportWriter.write(currentReview, request);
            synchronizeCaches(written);
            return createResult(currentReview, request, written);
        } finally {
            importLock.unlock();
        }
    }

    /**
     * What was saved is taken over by the caches once it is saved for good. Something which was already here is changed
     * in place, like any other change of it: whoever holds it - a page, a template which runs it - goes on with the
     * same object.
     */
    private void synchronizeCaches(Written written) {
        written.getPlaceholders().forEach(saved -> workflowPlaceholderService.getAllItems()
            .filter(cached -> cached.getName().equals(saved.getName()))
            .findFirst()
            .ifPresentOrElse(cached -> cached.setValue(saved.getValue()), () -> workflowPlaceholderService.create(saved, false)));
        written.getParameters().forEach(saved -> workflowParameterService.getAllItems()
            .filter(cached -> cached.getName().equals(saved.getName()))
            .findFirst()
            .ifPresentOrElse(cached -> {
                cached.setTypeAndValue(saved);
                workflowTemplateService.propagateInternalUpdate(cached);
            }, () -> workflowParameterService.create(saved, false)));
        written.getDefinitions().forEach(saved -> workflowDefinitionService.getAllItems()
            .filter(cached -> cached.getId().equals(saved.getId()))
            .findFirst()
            .ifPresentOrElse(cached -> {
                cached.setNameAndScript(saved);
                workflowTemplateService.propagateInternalUpdate(cached);
            }, () -> workflowDefinitionService.create(saved, false)));
        // a template comes last: it is scheduled as soon as the cache has it, and runs what the caches hold by then
        written.getTemplates().forEach(saved -> workflowTemplateService.getAllItems()
            .filter(cached -> cached.getId().equals(saved.getId()))
            .findFirst()
            .ifPresentOrElse(cached -> workflowTemplateService.applyInternalUpdate(cached, saved), () -> workflowTemplateService.create(saved, false)));
    }

    private WorkflowImportResult createResult(WorkflowImportReview review, WorkflowImportRequest request, Written written) {
        final var outcomes = new ArrayList<Outcome>();
        final var todos = new ArrayList<Todo>();
        review.items().forEach(item -> {
            final var imported = review.actionOf(item, request) == Action.IMPORT;
            final var outcome = new Outcome(item.kind(), item.name(), item.template(), written.getTargets().getOrDefault(item.key(), item.target()),
                findChange(item, imported));
            outcomes.add(outcome);
            if (imported) {
                findTodo(review, request, item, outcome).ifPresent(todos::add);
            }
        });
        return new WorkflowImportResult(List.copyOf(outcomes), List.copyOf(todos));
    }

    private Change findChange(WorkflowImportItem item, boolean imported) {
        if (item.state() == State.NEW) {
            return imported ? Change.CREATED : Change.SKIPPED;
        }
        return imported ? Change.OVERWRITTEN : Change.KEPT;
    }

    /**
     * @return what is left to do about an item which was imported, when there is something.
     */
    private Optional<Todo> findTodo(WorkflowImportReview review, WorkflowImportRequest request, WorkflowImportItem item, Outcome outcome) {
        if (isPausedTemplate(review, request, item)) {
            return Optional.of(new Todo(Todo.Type.TEMPLATE_PAUSED, outcome, null));
        }
        if (item.has(WorkflowImportNote.Type.VALUE_MISSING) && StringUtils.isEmpty(request.values().get(item.key()))) {
            return Optional.of(new Todo(Todo.Type.VALUE_MISSING, outcome, null));
        }
        return item.notes().stream()
            .filter(note -> note.type() == WorkflowImportNote.Type.UNKNOWN_COMMANDS)
            .findFirst()
            .map(note -> new Todo(Todo.Type.UNKNOWN_COMMANDS, outcome, String.join(NAME_SEPARATOR, note.arguments())));
    }

    private boolean isPausedTemplate(WorkflowImportReview review, WorkflowImportRequest request, WorkflowImportItem item) {
        return item.kind() == WorkflowTransferKind.TEMPLATE && item.state() == State.NEW && request.pausingNewTemplates()
            && review.bundle().templates().stream().anyMatch(template -> template.enabled() && item.name().equals(template.name()));
    }

}
