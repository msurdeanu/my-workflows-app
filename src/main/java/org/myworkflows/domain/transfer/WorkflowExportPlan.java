package org.myworkflows.domain.transfer;

import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import static org.myworkflows.domain.transfer.WorkflowTransferContent.definitionsOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.isOwn;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.ownNameOf;
import static org.myworkflows.domain.transfer.WorkflowTransferContent.parametersOf;

/**
 * What an export writes for a given selection: the picked items and, when the selection asks for it, what they need -
 * the definitions and the parameters of a picked template, the placeholders an exported definition names. It also says
 * why an item is part of the export, for the page which shows the selection.
 *
 * <p>A parameter of a single template is no item of its own: it is exported with its template, and only with it.</p>
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowExportPlan {

    private final WorkflowExportSelection selection;

    private final List<WorkflowTemplate> templates;

    private final List<WorkflowDefinition> definitions;

    private final List<WorkflowParameter> parameters;

    private final List<WorkflowPlaceholder> placeholders;

    // the names of the templates which need a definition or a parameter, and of the definitions which need a placeholder
    private final Map<Integer, List<String>> definitionNeeds = new HashMap<>();
    private final Map<String, List<String>> parameterNeeds = new HashMap<>();
    private final Map<String, List<String>> placeholderNeeds = new HashMap<>();

    // the template each parameter of a single template is meant for, by the name of the parameter
    private final Map<String, WorkflowTemplate> owners = new HashMap<>();

    private final WorkflowBundle bundle;

    private WorkflowExportPlan(WorkflowExportSelection selection, WorkflowTransferContent content) {
        this.selection = selection;
        content.getTemplates().forEach(template -> parametersOf(template).stream()
            .filter(parameter -> isOwn(template, parameter))
            .forEach(parameter -> owners.put(parameter.getName(), template)));

        templates = content.getTemplates().stream().filter(template -> selection.templates().contains(template.getId())).toList();
        if (selection.withDependencies()) {
            templates.forEach(this::addNeedsOf);
        }
        definitions = content.getDefinitions().stream()
            .filter(definition -> selection.definitions().contains(definition.getId()) || definitionNeeds.containsKey(definition.getId()))
            .toList();
        parameters = content.getParameters().stream()
            .filter(parameter -> parameterNeeds.containsKey(parameter.getName())
                || selection.parameters().contains(parameter.getName()) && !owners.containsKey(parameter.getName()))
            .toList();
        if (selection.withDependencies()) {
            definitions.forEach(definition -> content.placeholderNamesOf(definition)
                .forEach(name -> placeholderNeeds.computeIfAbsent(name, _ -> new ArrayList<>()).add(definition.getName())));
        }
        placeholders = content.getPlaceholders().stream()
            .filter(placeholder -> selection.placeholders().contains(placeholder.getName()) || placeholderNeeds.containsKey(placeholder.getName()))
            .toList();
        bundle = createBundle();
    }

    public static WorkflowExportPlan of(WorkflowExportSelection selection, WorkflowTransferContent content) {
        return new WorkflowExportPlan(selection, content);
    }

    public Entry entryOf(WorkflowTemplate template) {
        return new Entry(selection.templates().contains(template.getId()), false, List.of(), null);
    }

    public Entry entryOf(WorkflowDefinition definition) {
        final var neededBy = definitionNeeds.getOrDefault(definition.getId(), List.of());
        return new Entry(!neededBy.isEmpty() || selection.definitions().contains(definition.getId()), !neededBy.isEmpty(), neededBy, null);
    }

    public Entry entryOf(WorkflowParameter parameter) {
        final var neededBy = parameterNeeds.getOrDefault(parameter.getName(), List.of());
        final var owner = owners.get(parameter.getName());
        if (owner != null) {
            return new Entry(!neededBy.isEmpty() || selection.templates().contains(owner.getId()), true, neededBy, owner.getName());
        }
        return new Entry(!neededBy.isEmpty() || selection.parameters().contains(parameter.getName()), !neededBy.isEmpty(), neededBy, null);
    }

    public Entry entryOf(WorkflowPlaceholder placeholder) {
        final var neededBy = placeholderNeeds.getOrDefault(placeholder.getName(), List.of());
        return new Entry(!neededBy.isEmpty() || selection.placeholders().contains(placeholder.getName()), !neededBy.isEmpty(), neededBy, null);
    }

    /**
     * @return whether the value of the given parameter stays out of the file, as the selection keeps the secrets out.
     */
    public boolean leavesOutValueOf(WorkflowParameter parameter) {
        return !selection.withSecrets() && new WorkflowBundle.Parameter(parameter.getName(), parameter.getType(), null).holdsSecret();
    }

    public boolean leavesOutValueOf(WorkflowPlaceholder placeholder) {
        return !selection.withSecrets() && WorkflowBundle.namesSecret(placeholder.getName());
    }

    /**
     * @return how many items of the given kind are exported. A parameter of a single template is a parameter too.
     */
    public int count(WorkflowTransferKind kind) {
        return switch (kind) {
            case TEMPLATE -> bundle.templates().size();
            case DEFINITION -> bundle.definitions().size();
            case PARAMETER -> bundle.parameters().size() + bundle.templates().stream().mapToInt(template -> template.ownParameters().size()).sum();
            case PLACEHOLDER -> bundle.placeholders().size();
        };
    }

    public int size() {
        return bundle.size();
    }

    /**
     * @return how many values are not written to the file because they are secrets.
     */
    public long countLeftOutValues() {
        final var ownParameters = bundle.templates().stream().flatMap(template -> template.ownParameters().stream());
        return Stream.concat(bundle.parameters().stream(), ownParameters).filter(parameter -> parameter.value() == null).count()
            + bundle.placeholders().stream().filter(placeholder -> placeholder.value() == null).count();
    }

    /**
     * An import finds a definition and a template by their name, so two of them cannot go by the same name in one file.
     *
     * @param kind either {@link WorkflowTransferKind#TEMPLATE} or {@link WorkflowTransferKind#DEFINITION}.
     * @return the names more than one exported item of the given kind has.
     */
    public List<String> findDuplicateNames(WorkflowTransferKind kind) {
        if (kind == WorkflowTransferKind.TEMPLATE) {
            return findDuplicates(templates.stream().map(WorkflowTemplate::getName).toList());
        }
        final var duplicates = new LinkedHashSet<>(findDuplicates(definitions.stream().map(WorkflowDefinition::getName).toList()));
        // a template names its definitions even when they are not exported with it
        templates.forEach(template -> duplicates.addAll(findDuplicates(definitionsOf(template).stream().map(WorkflowDefinition::getName).toList())));
        return List.copyOf(duplicates);
    }

    public boolean isExportable() {
        return size() > 0 && findDuplicateNames(WorkflowTransferKind.TEMPLATE).isEmpty() && findDuplicateNames(WorkflowTransferKind.DEFINITION).isEmpty();
    }

    public WorkflowBundle toBundle(WorkflowBundle.Origin origin) {
        return new WorkflowBundle(WorkflowBundle.FORMAT, origin, bundle.placeholders(), bundle.parameters(), bundle.definitions(), bundle.templates());
    }

    private void addNeedsOf(WorkflowTemplate template) {
        definitionsOf(template).forEach(definition -> definitionNeeds.computeIfAbsent(definition.getId(), _ -> new ArrayList<>()).add(template.getName()));
        parametersOf(template).stream()
            .filter(parameter -> !isOwn(template, parameter))
            .forEach(parameter -> parameterNeeds.computeIfAbsent(parameter.getName(), _ -> new ArrayList<>()).add(template.getName()));
    }

    private WorkflowBundle createBundle() {
        return new WorkflowBundle(WorkflowBundle.FORMAT, null,
            placeholders.stream()
                .map(placeholder -> new WorkflowBundle.Placeholder(placeholder.getName(),
                    leavesOutValueOf(placeholder) ? null : StringUtils.defaultString(placeholder.getValue())))
                .toList(),
            parameters.stream().map(parameter -> toParameter(parameter.getName(), parameter)).toList(),
            definitions.stream().map(definition -> new WorkflowBundle.Definition(definition.getName(), definition.getScript())).toList(),
            templates.stream().map(this::toTemplate).toList());
    }

    private WorkflowBundle.Template toTemplate(WorkflowTemplate template) {
        return new WorkflowBundle.Template(template.getName(), StringUtils.trimToNull(template.getCron()), template.isEnabled(),
            definitionsOf(template).stream().map(WorkflowDefinition::getName).toList(),
            parametersOf(template).stream().filter(parameter -> !isOwn(template, parameter)).map(WorkflowParameter::getName).toList(),
            parametersOf(template).stream()
                .filter(parameter -> isOwn(template, parameter))
                .map(parameter -> toParameter(ownNameOf(template, parameter), parameter))
                .sorted(Comparator.comparing(WorkflowBundle.Parameter::name))
                .toList());
    }

    /**
     * @param name the name the parameter goes by in the file, which is not the one it is saved under when it is meant
     *     for a single template.
     */
    private WorkflowBundle.Parameter toParameter(String name, WorkflowParameter parameter) {
        final var withoutValue = new WorkflowBundle.Parameter(name, parameter.getType(), null);
        return !selection.withSecrets() && withoutValue.holdsSecret()
            ? withoutValue
            : new WorkflowBundle.Parameter(name, parameter.getType(), StringUtils.defaultString(parameter.getValue()));
    }

    private static List<String> findDuplicates(List<String> names) {
        final var seen = new HashSet<String>();
        return names.stream().filter(name -> !seen.add(name)).filter(Objects::nonNull).distinct().toList();
    }

    /**
     * How an item stands in an export.
     *
     * @param included whether the item is exported.
     * @param locked whether that is decided by something else than a pick of the item itself.
     * @param neededBy the names of the exported items which need this one.
     * @param owner the name of the template a parameter is meant for only, missing for anything else.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    public record Entry(boolean included, boolean locked, List<String> neededBy, String owner) {

    }

}
