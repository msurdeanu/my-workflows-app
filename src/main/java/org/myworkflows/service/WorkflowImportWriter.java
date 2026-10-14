package org.myworkflows.service;

import jakarta.transaction.Transactional;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.domain.transfer.WorkflowImportItem;
import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportItem.State;
import org.myworkflows.domain.transfer.WorkflowImportRequest;
import org.myworkflows.domain.transfer.WorkflowImportReview;
import org.myworkflows.domain.transfer.WorkflowTransferKind;
import org.myworkflows.exception.WorkflowTransferException;
import org.myworkflows.exception.WorkflowTransferException.Reason;
import org.myworkflows.repository.WorkflowDefinitionRepository;
import org.myworkflows.repository.WorkflowParameterRepository;
import org.myworkflows.repository.WorkflowPlaceholderRepository;
import org.myworkflows.repository.WorkflowTemplateRepository;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toCollection;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.storedNameOf;

/**
 * Saves what an import brings, in one transaction: either all of it reaches the database or none of it does. It works
 * on what it reads from the database itself and touches nothing which is cached, so that an import which fails halfway
 * leaves no trace anywhere: bringing the caches in line with what was saved is left to whoever called.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Service
@RequiredArgsConstructor
public class WorkflowImportWriter {

    private final WorkflowTemplateRepository workflowTemplateRepository;

    private final WorkflowDefinitionRepository workflowDefinitionRepository;

    private final WorkflowParameterRepository workflowParameterRepository;

    private final WorkflowPlaceholderRepository workflowPlaceholderRepository;

    /**
     * @param review the file, compared with what is here by now.
     * @param request what was chosen for the items of the file. It is taken as it is: whether it can be applied is
     *     checked by whoever calls.
     * @return what was saved.
     */
    @Transactional
    public Written write(WorkflowImportReview review, WorkflowImportRequest request) {
        return new Writing(review, request).run();
    }

    /**
     * What an import saved. A template comes with its definitions and its parameters, as they are saved by now.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @Getter
    public static final class Written {

        private final List<WorkflowPlaceholder> placeholders = new ArrayList<>();

        private final List<WorkflowParameter> parameters = new ArrayList<>();

        private final List<WorkflowDefinition> definitions = new ArrayList<>();

        private final List<WorkflowTemplate> templates = new ArrayList<>();

        // what each saved item is here from now on, by the key of the item: see WorkflowImportItem#target()
        private final Map<String, String> targets = new HashMap<>();

    }

    /**
     * One import on its way: the items are saved kind by kind, a template last, as it names the other ones.
     *
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @RequiredArgsConstructor
    private final class Writing {

        private final Written written = new Written();

        // what this import saved so far, by name: a template finds there what comes with it
        private final Map<String, WorkflowDefinition> savedDefinitions = new HashMap<>();
        private final Map<String, WorkflowParameter> savedParameters = new HashMap<>();

        private final WorkflowImportReview review;

        private final WorkflowImportRequest request;

        private Written run() {
            final var bundle = review.bundle();
            bundle.placeholders().forEach(placeholder -> findImported(WorkflowTransferKind.PLACEHOLDER, placeholder.name(), null)
                .ifPresent(item -> savePlaceholder(placeholder, item)));
            bundle.parameters().forEach(parameter -> findImported(WorkflowTransferKind.PARAMETER, parameter.name(), null)
                .ifPresent(item -> savedParameters.put(parameter.name(), saveParameter(parameter, item, parameter.name()))));
            bundle.definitions().forEach(definition -> findImported(WorkflowTransferKind.DEFINITION, definition.name(), null)
                .ifPresent(item -> saveDefinition(definition, item)));
            bundle.templates().forEach(this::saveTemplateAndOwnParameters);
            return written;
        }

        private Optional<WorkflowImportItem> findImported(WorkflowTransferKind kind, String name, String template) {
            return review.find(kind, name, template).filter(item -> review.actionOf(item, request) == Action.IMPORT);
        }

        private void savePlaceholder(WorkflowBundle.Placeholder placeholder, WorkflowImportItem item) {
            final var valueHere = review.here().findPlaceholder(placeholder.name()).map(WorkflowPlaceholder::getValue);
            written.getPlaceholders().add(workflowPlaceholderRepository.save(
                WorkflowPlaceholder.of(placeholder.name(), chooseValue(placeholder.value(), valueHere, item))));
            written.getTargets().put(item.key(), placeholder.name());
        }

        /**
         * @param storedName the name the parameter is saved under, which is not the one it has in the file when it is
         *     meant for a single template.
         */
        private WorkflowParameter saveParameter(WorkflowBundle.Parameter parameter, WorkflowImportItem item, String storedName) {
            final var valueHere = review.here().findParameter(storedName).map(WorkflowParameter::getValue);
            final var saved = workflowParameterRepository.save(
                WorkflowParameter.of(storedName, parameter.type(), chooseValue(parameter.value(), valueHere, item)));
            written.getParameters().add(saved);
            written.getTargets().put(item.key(), storedName);
            return saved;
        }

        /**
         * @return the value of the file, unless the file left it out: then what is here keeps its value, and what is
         *     new gets the one which was typed for it - or none.
         */
        private String chooseValue(String valueInFile, Optional<String> valueHere, WorkflowImportItem item) {
            return ofNullable(valueInFile)
                .or(() -> valueHere)
                .orElseGet(() -> request.values().getOrDefault(item.key(), StringUtils.EMPTY));
        }

        private void saveDefinition(WorkflowBundle.Definition definition, WorkflowImportItem item) {
            final WorkflowDefinition saved;
            if (item.state() == State.NEW) {
                saved = workflowDefinitionRepository.save(WorkflowDefinition.of(definition.name(), definition.script()));
            } else {
                final var existing = workflowDefinitionRepository.findById(Integer.valueOf(item.target())).orElseThrow(Writing::createGoneException);
                existing.setScript(definition.script());
                saved = workflowDefinitionRepository.save(existing);
            }
            savedDefinitions.put(definition.name(), saved);
            written.getDefinitions().add(saved);
            written.getTargets().put(item.key(), String.valueOf(saved.getId()));
        }

        /**
         * A parameter of a single template is saved under a name which ends with the id of its template, hence after
         * its template, which is saved once more to take it in.
         */
        private void saveTemplateAndOwnParameters(WorkflowBundle.Template template) {
            final var item = review.find(WorkflowTransferKind.TEMPLATE, template.name(), null).orElseThrow(Writing::createGoneException);
            final var saved = review.actionOf(item, request) == Action.IMPORT ? saveTemplate(template, item) : null;
            // a template which is not imported is the one which is here, when there is one
            final var templateId = ofNullable(saved).map(WorkflowTemplate::getId).or(() -> ofNullable(item.target()).map(Integer::valueOf));
            final var ownParameters = templateId.map(id -> saveOwnParameters(template, id)).orElseGet(List::of);
            if (saved != null) {
                final var parameters = new ArrayList<>(saved.getWorkflowParameters());
                parameters.addAll(ownParameters);
                saved.setWorkflowParameters(parameters);
                written.getTemplates().add(workflowTemplateRepository.save(saved));
                written.getTargets().put(item.key(), String.valueOf(saved.getId()));
            }
        }

        /**
         * @return the template with what it shares with other templates. A template which is already here keeps its
         *     activation, and its name is the one it was found by.
         */
        private WorkflowTemplate saveTemplate(WorkflowBundle.Template template, WorkflowImportItem item) {
            final var definitions = template.definitions().stream().distinct().map(this::findDefinition).collect(toCollection(ArrayList::new));
            final var parameters = template.parameters().stream().distinct().map(this::findParameter).collect(toCollection(ArrayList::new));
            if (item.state() == State.NEW) {
                final var created = WorkflowTemplate.of(template.name(), definitions, parameters);
                created.setCron(StringUtils.trimToNull(template.cron()));
                if (template.enabled() && !request.pausingNewTemplates()) {
                    created.toggleOnEnabling();
                }
                return workflowTemplateRepository.save(created);
            }

            final var existing = workflowTemplateRepository.findById(Integer.valueOf(item.target())).orElseThrow(Writing::createGoneException);
            existing.setCron(StringUtils.trimToNull(template.cron()));
            existing.setWorkflowDefinitions(definitions);
            existing.setWorkflowParameters(parameters);
            return existing;
        }

        /**
         * @return the parameters of the template alone which are saved from now on, whether this import saved them or
         *     they were here already.
         */
        private List<WorkflowParameter> saveOwnParameters(WorkflowBundle.Template template, int templateId) {
            final var ownParameters = new ArrayList<WorkflowParameter>();
            template.ownParameters().forEach(parameter -> {
                final var storedName = storedNameOf(templateId, parameter.name());
                findImported(WorkflowTransferKind.PARAMETER, parameter.name(), template.name())
                    .map(item -> saveParameter(parameter, item, storedName))
                    .or(() -> workflowParameterRepository.findById(storedName))
                    .ifPresent(ownParameters::add);
            });
            return ownParameters;
        }

        private WorkflowDefinition findDefinition(String name) {
            return ofNullable(savedDefinitions.get(name))
                .or(() -> review.here().findDefinitions(name).stream().findFirst().flatMap(item -> workflowDefinitionRepository.findById(item.getId())))
                .orElseThrow(Writing::createGoneException);
        }

        private WorkflowParameter findParameter(String name) {
            return ofNullable(savedParameters.get(name))
                .or(() -> workflowParameterRepository.findById(name))
                .orElseThrow(Writing::createGoneException);
        }

        /**
         * Something the import counted on is not in the database anymore, so the whole of it is given up.
         */
        private static WorkflowTransferException createGoneException() {
            return new WorkflowTransferException(Reason.INSTANCE_CHANGED);
        }

    }

}
