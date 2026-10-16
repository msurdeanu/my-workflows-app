package org.myworkflows.restapi.dto;

/**
 * @param name the name of a new placeholder. A placeholder cannot be renamed, so an update takes the name of its URL.
 * @param value an empty text when a new placeholder is created without one. An update needs it, for the reason a
 *     parameter does.
 * @author Mihai Surdeanu
 * @since 2.2
 */
public record WorkflowPlaceholderRequest(String name, String value) {

}
