package org.myworkflows.domain.transfer;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.AbstractSubCommand;
import org.myworkflows.domain.command.UnknownCommand;
import org.myworkflows.domain.transfer.WorkflowImportItem.Action;
import org.myworkflows.domain.transfer.WorkflowImportItem.State;
import org.myworkflows.domain.transfer.WorkflowImportNote.Type;
import org.myworkflows.serializer.SerializerFactory;
import org.myworkflows.util.PlaceholderUtil;
import org.springframework.scheduling.support.CronExpression;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.util.Optional.ofNullable;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.definitionsOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.isOwn;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.ownNameOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.parametersOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.storedNameOf;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.DEFINITION;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.PARAMETER;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.PLACEHOLDER;
import static org.myworkflows.domain.transfer.WorkflowTransferKind.TEMPLATE;

/**
 * Compares a file which is about to be imported with what the instance holds. An item of the file is the one here which
 * goes by the same name, and it is new when nothing here does. Nothing is changed by looking.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@RequiredArgsConstructor
public final class WorkflowImportReviewer {

    public static final String PARAMETER_NAME_ALLOWED_CHARS = "[a-zA-Z0-9_.]";

    private static final Pattern PARAMETER_NAME_PATTERN = Pattern.compile(PARAMETER_NAME_ALLOWED_CHARS + "+");

    private static final Pattern PLACEHOLDER_NAME_PATTERN = Pattern.compile(PlaceholderUtil.PLACEHOLDER_NAME_PATTERN);

    // the notes which say that an item is not here what it is in the file
    private static final Set<Type> DIFFERENCES = EnumSet.of(Type.CRON, Type.DEFINITIONS, Type.PARAMETERS, Type.SCRIPT, Type.TYPE_CHANGE, Type.VALUE,
        Type.SECRET_VALUE);

    private static final String NAME_SEPARATOR = ", ";

    private final WorkflowBundle bundle;

    private final WorkflowTransferContent here;

    /**
     * Finds what keeps the given choices from being applied: an item which cannot be imported as it is, or a template
     * which would be left without something it names.
     *
     * @return the problems, none when the import can go on.
     */
    public static List<WorkflowImportProblem> check(WorkflowImportReview review, WorkflowImportRequest request) {
        final var problems = new ArrayList<WorkflowImportProblem>();
        review.items().stream().filter(item -> review.actionOf(item, request) == Action.IMPORT).forEach(item -> {
            if (item.isInvalid()) {
                problems.add(WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_INVALID, item.kind(), item.name()));
            }
            findTypedValueError(review, item, request)
                .ifPresent(error -> problems.add(new WorkflowImportProblem(WorkflowImportProblem.Type.VALUE_INVALID, item.kind(), item.name(), error)));
        });
        review.bundle().templates().stream().filter(template -> review.isImported(TEMPLATE, template.name(), request)).forEach(template -> {
            template.definitions().stream().distinct()
                .filter(name -> review.here().findDefinitions(name).isEmpty() && !review.isImported(DEFINITION, name, request))
                .forEach(name -> problems.add(new WorkflowImportProblem(WorkflowImportProblem.Type.DEFINITION_MISSING, TEMPLATE, template.name(), name)));
            template.parameters().stream().distinct()
                .filter(name -> review.here().findParameter(name).isEmpty() && !review.isImported(PARAMETER, name, request))
                .forEach(name -> problems.add(new WorkflowImportProblem(WorkflowImportProblem.Type.PARAMETER_MISSING, TEMPLATE, template.name(), name)));
        });
        return problems;
    }

    public WorkflowImportReview review() {
        final var problems = findProblems();
        if (!problems.isEmpty()) {
            return new WorkflowImportReview(bundle, here, List.of(), problems);
        }

        final var items = new ArrayList<WorkflowImportItem>();
        bundle.templates().forEach(template -> items.add(reviewTemplate(template)));
        bundle.definitions().forEach(definition -> items.add(reviewDefinition(definition)));
        bundle.parameters().forEach(parameter -> items.add(reviewParameter(parameter, null, parameter.name())));
        bundle.templates().forEach(template -> template.ownParameters().forEach(parameter -> items.add(reviewOwnParameter(template, parameter))));
        bundle.placeholders().forEach(placeholder -> items.add(reviewPlaceholder(placeholder)));
        return new WorkflowImportReview(bundle, here, List.copyOf(items), List.of());
    }

    private static Optional<String> findTypedValueError(WorkflowImportReview review, WorkflowImportItem item, WorkflowImportRequest request) {
        final var typedValue = request.values().get(item.key());
        if (StringUtils.isEmpty(typedValue) || !item.has(Type.VALUE_MISSING)) {
            return Optional.empty();
        }
        return review.parameterOf(item).flatMap(parameter -> WorkflowParameter.validateTypeAndValue(parameter.type(), typedValue));
    }

    private List<WorkflowImportProblem> findProblems() {
        final var problems = new ArrayList<WorkflowImportProblem>();
        addUnusableItems(problems);
        addDuplicates(problems, TEMPLATE, bundle.templates().stream().map(WorkflowBundle.Template::name));
        addDuplicates(problems, DEFINITION, bundle.definitions().stream().map(WorkflowBundle.Definition::name));
        addDuplicates(problems, PARAMETER, bundle.parameters().stream().map(WorkflowBundle.Parameter::name));
        bundle.templates().forEach(template -> addDuplicates(problems, PARAMETER, template.ownParameters().stream().map(WorkflowBundle.Parameter::name)));
        addDuplicates(problems, PLACEHOLDER, bundle.placeholders().stream().map(WorkflowBundle.Placeholder::name));
        addAmbiguousNames(problems);
        return problems;
    }

    private void addUnusableItems(List<WorkflowImportProblem> problems) {
        bundle.templates().stream()
            .filter(template -> StringUtils.isBlank(template.name()))
            .forEach(template -> problems.add(unusable(TEMPLATE, template.name())));
        bundle.definitions().stream()
            .filter(definition -> StringUtils.isBlank(definition.name()) || definition.script() == null)
            .forEach(definition -> problems.add(unusable(DEFINITION, definition.name())));
        Stream.concat(bundle.parameters().stream(), bundle.templates().stream().flatMap(template -> template.ownParameters().stream()))
            .filter(parameter -> parameter.type() == null || !matches(PARAMETER_NAME_PATTERN, parameter.name()))
            .forEach(parameter -> problems.add(unusable(PARAMETER, parameter.name())));
        bundle.placeholders().stream()
            .filter(placeholder -> !matches(PLACEHOLDER_NAME_PATTERN, placeholder.name()))
            .forEach(placeholder -> problems.add(unusable(PLACEHOLDER, placeholder.name())));
    }

    private void addDuplicates(List<WorkflowImportProblem> problems, WorkflowTransferKind kind, Stream<String> names) {
        final var seen = new HashSet<String>();
        names.filter(Objects::nonNull)
            .filter(name -> !seen.add(name))
            .distinct()
            .forEach(name -> problems.add(WorkflowImportProblem.of(WorkflowImportProblem.Type.DUPLICATE_IN_FILE, kind, name)));
    }

    /**
     * A name of the file which two items here go by cannot be matched: the file means one of them, and does not say
     * which. A definition is also named by the templates of the file, whether it comes with them or not.
     */
    private void addAmbiguousNames(List<WorkflowImportProblem> problems) {
        bundle.templates().stream()
            .map(WorkflowBundle.Template::name)
            .filter(name -> here.findTemplates(name).size() > 1)
            .forEach(name -> problems.add(WorkflowImportProblem.of(WorkflowImportProblem.Type.AMBIGUOUS_HERE, TEMPLATE, name)));
        Stream.concat(bundle.definitions().stream().map(WorkflowBundle.Definition::name),
                bundle.templates().stream().flatMap(template -> template.definitions().stream()))
            .distinct()
            .filter(name -> here.findDefinitions(name).size() > 1)
            .forEach(name -> problems.add(WorkflowImportProblem.of(WorkflowImportProblem.Type.AMBIGUOUS_HERE, DEFINITION, name)));
    }

    private WorkflowImportItem reviewTemplate(WorkflowBundle.Template template) {
        final var existing = here.findTemplates(template.name()).stream().findFirst();
        final var notes = new ArrayList<WorkflowImportNote>();
        existing.ifPresentOrElse(item -> addDifferences(notes, item, template),
            () -> notes.add(WorkflowImportNote.of(Type.CONTENT, String.valueOf(template.definitions().stream().distinct().count()),
                String.valueOf(template.parameters().stream().distinct().count() + template.ownParameters().size()))));
        final var state = stateOf(existing.isPresent(), notes);
        // a cron which cannot be read would keep the template from being scheduled once it is here
        if (state != State.SAME && StringUtils.isNotBlank(template.cron()) && !CronExpression.isValidExpression(template.cron())) {
            notes.add(WorkflowImportNote.of(Type.CRON_INVALID, template.cron()));
        }
        return new WorkflowImportItem(TEMPLATE, template.name(), null, state, existing.map(item -> String.valueOf(item.getId())).orElse(null), false,
            List.copyOf(notes));
    }

    /**
     * The activation of a template is no difference: a template which is already here keeps the one it has.
     */
    private void addDifferences(List<WorkflowImportNote> notes, WorkflowTemplate existing, WorkflowBundle.Template template) {
        final var cronHere = StringUtils.trimToEmpty(existing.getCron());
        final var cronInFile = StringUtils.trimToEmpty(template.cron());
        if (!cronHere.equals(cronInFile)) {
            notes.add(WorkflowImportNote.of(Type.CRON, cronHere, cronInFile));
        }

        // the definitions run in the order they are listed, so another order is another template
        final var definitionsHere = definitionsOf(existing).stream().map(WorkflowDefinition::getName).toList();
        final var definitionsInFile = template.definitions().stream().distinct().toList();
        if (!definitionsHere.equals(definitionsInFile)) {
            notes.add(WorkflowImportNote.of(Type.DEFINITIONS, String.join(NAME_SEPARATOR, definitionsHere), String.join(NAME_SEPARATOR, definitionsInFile)));
        }

        final var sharedHere = toSortedNames(parametersOf(existing).stream().filter(parameter -> !isOwn(existing, parameter)).map(WorkflowParameter::getName));
        final var ownHere = toSortedNames(parametersOf(existing).stream().filter(parameter -> isOwn(existing, parameter))
            .map(parameter -> ownNameOf(existing, parameter)));
        final var sharedInFile = toSortedNames(template.parameters().stream());
        final var ownInFile = toSortedNames(template.ownParameters().stream().map(WorkflowBundle.Parameter::name));
        if (!sharedHere.equals(sharedInFile) || !ownHere.equals(ownInFile)) {
            notes.add(WorkflowImportNote.of(Type.PARAMETERS, joinNames(sharedHere, ownHere), joinNames(sharedInFile, ownInFile)));
        }
    }

    private WorkflowImportItem reviewDefinition(WorkflowBundle.Definition definition) {
        final var existing = here.findDefinitions(definition.name()).stream().findFirst();
        final var notes = new ArrayList<WorkflowImportNote>();
        existing.map(item -> SerializerFactory.toString(item.getScript(), StringUtils.EMPTY))
            .filter(script -> !script.equals(SerializerFactory.toString(definition.script(), StringUtils.EMPTY)))
            .ifPresent(_ -> notes.add(WorkflowImportNote.of(Type.SCRIPT)));
        final var state = stateOf(existing.isPresent(), notes);
        final var unknownCommands = findUnknownCommands(definition.script());
        if (state != State.SAME && !unknownCommands.isEmpty()) {
            notes.add(WorkflowImportNote.of(Type.UNKNOWN_COMMANDS, String.join(NAME_SEPARATOR, unknownCommands)));
        }
        return new WorkflowImportItem(DEFINITION, definition.name(), null, state, existing.map(item -> String.valueOf(item.getId())).orElse(null), false,
            List.copyOf(notes));
    }

    /**
     * A parameter of a single template is saved under a name which ends with the id of its template, so it can be here
     * only when its template is.
     */
    private WorkflowImportItem reviewOwnParameter(WorkflowBundle.Template template, WorkflowBundle.Parameter parameter) {
        final var storedName = here.findTemplates(template.name()).stream().findFirst().map(item -> storedNameOf(item.getId(), parameter.name()));
        return reviewParameter(parameter, template.name(), storedName.orElse(null));
    }

    private WorkflowImportItem reviewParameter(WorkflowBundle.Parameter parameter, String templateName, String storedName) {
        final var existing = ofNullable(storedName).flatMap(here::findParameter);
        final var secret = parameter.holdsSecret()
            || existing.filter(item -> new WorkflowBundle.Parameter(item.getName(), item.getType(), null).holdsSecret()).isPresent();
        final var notes = new ArrayList<WorkflowImportNote>();
        existing.ifPresentOrElse(item -> {
            if (item.getType() != parameter.type()) {
                notes.add(WorkflowImportNote.of(Type.TYPE_CHANGE, ofNullable(item.getType()).map(WorkflowParameterType::getValue).orElse(StringUtils.EMPTY),
                    parameter.type().getValue()));
            }
            addValueDifference(notes, item.getValue(), parameter.value(), secret);
        }, () -> {
            notes.add(WorkflowImportNote.of(Type.TYPE, parameter.type().getValue()));
            if (parameter.value() == null) {
                notes.add(WorkflowImportNote.of(Type.VALUE_MISSING));
            }
        });
        final var state = stateOf(existing.isPresent(), notes);
        if (state != State.SAME && parameter.value() != null) {
            WorkflowParameter.validateTypeAndValue(parameter.type(), parameter.value())
                .ifPresent(error -> notes.add(WorkflowImportNote.of(Type.VALUE_INVALID, error)));
        }
        return new WorkflowImportItem(PARAMETER, parameter.name(), templateName, state, existing.map(WorkflowParameter::getName).orElse(null), secret,
            List.copyOf(notes));
    }

    private WorkflowImportItem reviewPlaceholder(WorkflowBundle.Placeholder placeholder) {
        final var existing = here.findPlaceholder(placeholder.name());
        final var notes = new ArrayList<WorkflowImportNote>();
        existing.ifPresentOrElse(item -> addValueDifference(notes, item.getValue(), placeholder.value(), placeholder.holdsSecret()), () -> {
            if (placeholder.value() == null) {
                notes.add(WorkflowImportNote.of(Type.VALUE_MISSING));
            }
        });
        return new WorkflowImportItem(PLACEHOLDER, placeholder.name(), null, stateOf(existing.isPresent(), notes),
            existing.map(WorkflowPlaceholder::getName).orElse(null), placeholder.holdsSecret(), List.copyOf(notes));
    }

    /**
     * A value the file left out is no difference: what is here keeps its value.
     */
    private void addValueDifference(List<WorkflowImportNote> notes, String valueHere, String valueInFile, boolean secret) {
        if (valueInFile == null) {
            notes.add(WorkflowImportNote.of(Type.VALUE_KEPT));
        } else if (!valueInFile.equals(valueHere)) {
            notes.add(secret ? WorkflowImportNote.of(Type.SECRET_VALUE) : WorkflowImportNote.of(Type.VALUE, StringUtils.defaultString(valueHere), valueInFile));
        }
    }

    private static State stateOf(boolean existing, List<WorkflowImportNote> notes) {
        if (!existing) {
            return State.NEW;
        }
        return notes.stream().anyMatch(note -> DIFFERENCES.contains(note.type())) ? State.DIFFERENT : State.SAME;
    }

    /**
     * @return the classes of the commands of the script which are not known here, the ones run by another command included.
     */
    private static Set<String> findUnknownCommands(WorkflowDefinitionScript script) {
        final var types = new TreeSet<String>();
        Stream.of(script.getCommands(), script.getFinallyCommands())
            .filter(Objects::nonNull)
            .flatMap(List::stream)
            .forEach(command -> addUnknownCommands(types, command));
        return types;
    }

    private static void addUnknownCommands(Set<String> types, AbstractCommand command) {
        if (command instanceof UnknownCommand unknownCommand) {
            types.add(String.valueOf(unknownCommand.getType()));
        }
        if (command instanceof AbstractSubCommand subCommand) {
            ofNullable(subCommand.getSubcommands()).orElse(List.of()).forEach(subcommand -> addUnknownCommands(types, subcommand));
        }
    }

    private static WorkflowImportProblem unusable(WorkflowTransferKind kind, String name) {
        return WorkflowImportProblem.of(WorkflowImportProblem.Type.ITEM_UNUSABLE, kind, StringUtils.defaultString(name));
    }

    private static boolean matches(Pattern pattern, String name) {
        return name != null && pattern.matcher(name).matches();
    }

    private static List<String> toSortedNames(Stream<String> names) {
        return names.distinct().sorted().toList();
    }

    private static String joinNames(List<String> sharedNames, List<String> ownNames) {
        return String.join(NAME_SEPARATOR, Stream.concat(sharedNames.stream(), ownNames.stream()).sorted().toList());
    }

}
