package org.myworkflows.service;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowDefinitionValidatorServiceTest {

    @Test
    public void whenWorkflowIsValidThenNoIssueIsDetected() {
        // given
        final var workflowAsString = """
            {
              "commands" : [ {
                "@type" : "sleep",
                "name" : "Sleep with placeholder",
                "inputs" : [ {
                  "name" : "sleepTime",
                  "value" : 1000
                } ]
              }, {
                "@type" : "print",
                "name" : "Print placeholder value",
                "inputs" : [ {
                  "name" : "keys",
                  "value" : [ "sleepTime" ]
                } ]
              } ]
            }
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(0, validationMessages.size());
    }

    @Test
    public void whenCommandNameIsMissingThenAProperMessageIsReturned() {
        // given
        final var workflowAsString = """
            {
              "commands" : [ {
                "@type" : "sleep",
                "inputs" : [ {
                  "name" : "sleepTime",
                  "value" : 1000
                } ]
              }, {
                "@type" : "print",
                "name" : "Print placeholder value",
                "inputs" : [ {
                  "name" : "keys",
                  "value" : [ "sleepTime" ]
                } ]
              } ]
            }
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(1, validationMessages.size());
        assertEquals("$.commands[0]: required property 'name' not found", validationMessages.iterator().next().getMessage());
    }

    @Test
    public void whenCommandHasUnknownPropertiesThenAProperMessageIsReturned() {
        // given
        final var workflowAsString = """
            {
              "commands" : [ {
                "@type" : "sleep",
                "name" : "Sleep with placeholder",
                "inputs" : [ {
                  "name" : "sleepTime",
                  "value" : 1000
                } ]
              }, {
                "@type" : "print",
                "name" : "Print placeholder value",
                "_inputs" : [ {
                  "name" : "keys",
                  "value" : [ "sleepTime" ]
                } ]
              } ]
            }
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(1, validationMessages.size());
        assertEquals("$.commands[1]: property '_inputs' is not defined in the schema and the schema does not allow additional properties",
            validationMessages.iterator().next().getMessage());
    }

    @Test
    public void whenCommandHasCommentsThenNoIssueIsDetected() {
        // given
        final var workflowAsString = """
            {
              "commands" : [ {
                "@type" : "sleep",
                "name" : "Sleep with placeholder",
                "_comment1": "My first comment",
                "_comment2": "My second comment",
                "inputs" : [ {
                  "name" : "sleepTime",
                  "value" : 1000
                } ]
              }, {
                "@type" : "print",
                "name" : "Print placeholder value",
                "inputs" : [ {
                  "name" : "keys",
                  "value" : [ "sleepTime" ]
                } ]
              } ]
            }
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(0, validationMessages.size());
    }

}
