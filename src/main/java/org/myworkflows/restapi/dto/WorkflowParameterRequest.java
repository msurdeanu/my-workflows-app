package org.myworkflows.restapi.dto;

import org.myworkflows.domain.WorkflowParameterType;

/**
 * @param name the name of a new parameter. A parameter cannot be renamed, so an update takes the name of its URL.
 * @param type {@code STR} when a new parameter is created without one.
 * @param value an empty text when a new parameter is created without one. An update needs it: a secret is never
 *     answered with its value, so a missing value cannot mean "keep it as it is".
 * @author Mihai Surdeanu
 * @since 2.2
 */
public record WorkflowParameterRequest(String name, WorkflowParameterType type, String value) {

}
