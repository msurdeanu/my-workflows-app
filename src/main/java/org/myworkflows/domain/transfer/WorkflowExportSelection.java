package org.myworkflows.domain.transfer;

import java.util.Set;

/**
 * What a user picked to be exported.
 *
 * @param templates the ids of the picked templates.
 * @param definitions the ids of the picked definitions.
 * @param parameters the names of the picked parameters.
 * @param placeholders the names of the picked placeholders.
 * @param withDependencies whether what the picked items need comes along: the definitions and the parameters of a
 *     template, the placeholders a definition names.
 * @param withSecrets whether the value of a secret is written to the file.
 * @author Mihai Surdeanu
 * @since 2.1
 */
public record WorkflowExportSelection(Set<Integer> templates, Set<Integer> definitions, Set<String> parameters, Set<String> placeholders,
                                      boolean withDependencies, boolean withSecrets) {

    public WorkflowExportSelection {
        templates = Set.copyOf(templates);
        definitions = Set.copyOf(definitions);
        parameters = Set.copyOf(parameters);
        placeholders = Set.copyOf(placeholders);
    }

}
