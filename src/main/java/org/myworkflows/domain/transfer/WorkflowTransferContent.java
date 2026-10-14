package org.myworkflows.domain.transfer;

import lombok.AccessLevel;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowDefinition;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.WorkflowTemplate;
import org.myworkflows.serializer.SerializerFactory;
import org.myworkflows.util.PlaceholderUtil;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import static java.util.Optional.ofNullable;

/**
 * What an instance holds of the four kinds an export writes, as it was when somebody looked: an export picks from it and
 * an import is compared with it. Each kind is sorted by name.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@Getter
public final class WorkflowTransferContent {

    private static final String ID_SEPARATOR = ".";

    private final List<WorkflowTemplate> templates;

    private final List<WorkflowDefinition> definitions;

    private final List<WorkflowParameter> parameters;

    private final List<WorkflowPlaceholder> placeholders;

    // reading the placeholders of a definition means writing its script down, which is done once per definition
    @Getter(AccessLevel.NONE)
    private final Map<Integer, Set<String>> placeholderNames = new ConcurrentHashMap<>();

    public WorkflowTransferContent(Collection<WorkflowTemplate> templates, Collection<WorkflowDefinition> definitions,
                                   Collection<WorkflowParameter> parameters, Collection<WorkflowPlaceholder> placeholders) {
        this.templates = templates.stream()
            .sorted(Comparator.comparing(WorkflowTemplate::getName, String.CASE_INSENSITIVE_ORDER).thenComparing(WorkflowTemplate::getId))
            .toList();
        this.definitions = definitions.stream()
            .sorted(Comparator.comparing(WorkflowDefinition::getName, String.CASE_INSENSITIVE_ORDER).thenComparing(WorkflowDefinition::getId))
            .toList();
        this.parameters = parameters.stream()
            .sorted(Comparator.comparing(WorkflowParameter::getName, String.CASE_INSENSITIVE_ORDER).thenComparing(WorkflowParameter::getName))
            .toList();
        this.placeholders = placeholders.stream()
            .sorted(Comparator.comparing(WorkflowPlaceholder::getName))
            .toList();
    }

    /**
     * @return the name a parameter of a single template is saved under: the name its workflow gets it under, followed
     *     by the id of the template.
     */
    public static String storedNameOf(int templateId, String parameterName) {
        return parameterName + ID_SEPARATOR + templateId;
    }

    public static List<WorkflowDefinition> definitionsOf(WorkflowTemplate template) {
        return ofNullable(template.getWorkflowDefinitions()).orElse(List.of());
    }

    public static List<WorkflowParameter> parametersOf(WorkflowTemplate template) {
        return ofNullable(template.getWorkflowParameters()).orElse(List.of());
    }

    /**
     * @return whether the given parameter of the template is meant for that template only, which its name says by
     *     ending with the id of the template: `timeout.42` reaches the workflow of template 42 as `timeout`.
     */
    public static boolean isOwn(WorkflowTemplate template, WorkflowParameter parameter) {
        final var suffix = ID_SEPARATOR + template.getId();
        return parameter.getName().endsWith(suffix) && parameter.getName().length() > suffix.length();
    }

    /**
     * @return the name the workflow of the template gets one of its own parameters under.
     */
    public static String ownNameOf(WorkflowTemplate template, WorkflowParameter parameter) {
        return StringUtils.removeEnd(parameter.getName(), ID_SEPARATOR + template.getId());
    }

    /**
     * Names are not unique for templates, so there can be more than one.
     */
    public List<WorkflowTemplate> findTemplates(String name) {
        return templates.stream().filter(template -> template.getName().equals(name)).toList();
    }

    /**
     * Names are not unique for definitions, so there can be more than one.
     */
    public List<WorkflowDefinition> findDefinitions(String name) {
        return definitions.stream().filter(definition -> definition.getName().equals(name)).toList();
    }

    public Optional<WorkflowParameter> findParameter(String name) {
        return parameters.stream().filter(parameter -> parameter.getName().equals(name)).findFirst();
    }

    public Optional<WorkflowPlaceholder> findPlaceholder(String name) {
        return placeholders.stream().filter(placeholder -> placeholder.getName().equals(name)).findFirst();
    }

    /**
     * @return the template the given parameter is meant for only, when there is one.
     */
    public Optional<WorkflowTemplate> findOwner(WorkflowParameter parameter) {
        return templates.stream()
            .filter(template -> isOwn(template, parameter) && parametersOf(template).contains(parameter))
            .findFirst();
    }

    /**
     * @return the names of the placeholders the script of the given definition refers to, whether they exist or not.
     */
    public Set<String> placeholderNamesOf(WorkflowDefinition definition) {
        return placeholderNames.computeIfAbsent(definition.getId(),
            _ -> PlaceholderUtil.findPlaceholderNames(SerializerFactory.toString(definition.getScript(), StringUtils.EMPTY)));
    }

}
