package org.myworkflows.restapi.dto;

/**
 * What a copy of a definition or a template is made with. The whole body may be left out.
 *
 * @param name the name of the copy. Nothing for the first of {@code <name> (copy)}, {@code <name> (copy 2)}... which no
 *     item of the same kind has yet.
 * @param ownParameters whether the parameters a template has for itself alone are copied for the new one. Nothing for
 *     yes. A definition has none.
 * @author Mihai Surdeanu
 * @since 2.2
 */
public record WorkflowDuplicateRequest(String name, Boolean ownParameters) {

    public boolean isWithOwnParameters() {
        return !Boolean.FALSE.equals(ownParameters);
    }

}
