package org.myworkflows.domain.transfer;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowRunPrint;

import java.time.Instant;
import java.util.List;
import java.util.Objects;

/**
 * What an export writes and an import reads, on another instance most of the time. Nothing in it is an id, as an id
 * means something on the instance it comes from only: a template names its definitions and its parameters, and
 * whatever is imported is matched with what is already there by its name.
 *
 * @param format the version of this layout, for an import to tell a file it cannot read from one it can.
 * @author Mihai Surdeanu
 * @since 2.1
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonPropertyOrder({"format", "exported", "placeholders", "parameters", "definitions", "templates"})
public record WorkflowBundle(int format, Origin exported, List<Placeholder> placeholders, List<Parameter> parameters, List<Definition> definitions,
                             List<Template> templates) {

    public static final int FORMAT = 1;

    public WorkflowBundle {
        placeholders = withoutNulls(placeholders);
        parameters = withoutNulls(parameters);
        definitions = withoutNulls(definitions);
        templates = withoutNulls(templates);
    }

    /**
     * @return how many items the bundle holds, the parameters of a single template included.
     */
    public int size() {
        return placeholders.size() + parameters.size() + definitions.size() + templates.size()
            + templates.stream().mapToInt(template -> template.ownParameters().size()).sum();
    }

    /**
     * @return whether a parameter or a placeholder of this name is taken for a secret, by the rule a printed value is
     *     masked with: its name says so.
     */
    public static boolean namesSecret(String name) {
        return StringUtils.containsIgnoreCase(name, WorkflowRunPrint.PASSWORD);
    }

    private static <T> List<T> withoutNulls(List<T> items) {
        return items == null ? List.of() : items.stream().filter(Objects::nonNull).toList();
    }

    /**
     * Where a bundle comes from.
     *
     * @param by the name of the user who exported it.
     * @param version the version of the application which wrote it.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Origin(Instant at, String by, String version) {

    }

    /**
     * @param value missing when the placeholder holds a secret which was left out of the export.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Placeholder(String name, String value) {

        public boolean holdsSecret() {
            return namesSecret(name);
        }

    }

    /**
     * @param name the name of the parameter. For a parameter of a single template it is the name the workflow gets it
     *     under, without the id of the template it ends with where it is saved.
     * @param value missing when the parameter holds a secret which was left out of the export.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Parameter(String name, WorkflowParameterType type, String value) {

        public boolean holdsSecret() {
            return type == WorkflowParameterType.PASS || namesSecret(name);
        }

    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Definition(String name, WorkflowDefinitionScript script) {

    }

    /**
     * @param definitions the names of the definitions the template runs, in the order it runs them.
     * @param parameters the names of the parameters the template shares with other templates.
     * @param ownParameters the parameters which are meant for this template only, each of them whole: such a parameter
     *     is saved under a name which ends with the id of its template, so it cannot be told by its name elsewhere.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Template(String name, String cron, boolean enabled, List<String> definitions, List<String> parameters,
                           @JsonInclude(JsonInclude.Include.NON_EMPTY) List<Parameter> ownParameters) {

        public Template {
            definitions = withoutNulls(definitions);
            parameters = withoutNulls(parameters);
            ownParameters = withoutNulls(ownParameters);
        }

    }

}
