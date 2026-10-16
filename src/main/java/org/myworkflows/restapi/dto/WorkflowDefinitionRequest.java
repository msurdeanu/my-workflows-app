package org.myworkflows.restapi.dto;

/**
 * @param script the script as YAML - or as JSON, which YAML reads too. A definition can be created without one, and
 *     starts with no command then, but not updated without one.
 * @author Mihai Surdeanu
 * @since 2.2
 */
public record WorkflowDefinitionRequest(String name, String script) {

}
