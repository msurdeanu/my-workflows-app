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
            commands:
            - class: sleep
              name: Sleep command
              inputs:
              - name: sleep.time
                value: 1000
            - class: print
              name: Print command
              inputs:
              - name: print.keys
                value:
                - sleep.time
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(0, validationMessages.size());
    }

    @Test
    public void whenCommandNameIsMissingThenAProperMessageIsReturned() {
        // given
        final var workflowAsString = """
            commands:
            - class: sleep
              inputs:
              - name: sleep.time
                value: 1000
            - class: print
              name: Print command
              inputs:
              - name: print.keys
                value:
                - sleep.time
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(1, validationMessages.size());
        assertEquals("$.commands[0]: required property 'name' not found", validationMessages.getFirst().toString());
    }

    @Test
    public void whenCommandHasUnknownPropertiesThenAProperMessageIsReturned() {
        // given
        final var workflowAsString = """
            commands:
            - class: sleep
              name: Sleep command
              inputs:
              - name: sleep.time
                value: 1000
            - class: print
              name: Print command
              _inputs:
              - name: print.keys
                value:
                - sleep.time
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(1, validationMessages.size());
        assertEquals("$.commands[1]: property '_inputs' is not defined in the schema and the schema does not allow additional properties",
            validationMessages.getFirst().toString());
    }

    @Test
    public void whenCommandClassIsNotKnownThenAProperMessageIsReturned() {
        // given
        final var workflowAsString = """
            commands:
            - class: slep
              name: Sleep command
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(1, validationMessages.size());
        assertEquals("$.commands[0].class: does not have a value in the enumeration [\"approval\", \"database\", \"groovy\", \"httpRequest\", "
            + "\"java\", \"loop\", \"mail\", \"nothing\", \"print\", \"report\", \"sleep\", \"sshExec\", \"sshShell\", \"waitUntilSubPasses\"]",
            validationMessages.getFirst().toString());
    }

    @Test
    public void whenCommandWhichRunsOtherCommandsHasNoneThenAProperMessageIsReturned() {
        // given
        final var workflowAsString = """
            commands:
            - class: loop
              name: Loop command
            - class: waitUntilSubPasses
              name: Wait command
            - class: approval
              name: Approval command
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(2, validationMessages.size());
        assertEquals("$.commands[0]: required property 'subcommands' not found", validationMessages.getFirst().toString());
        assertEquals("$.commands[1]: required property 'subcommands' not found", validationMessages.getLast().toString());
    }

    @Test
    public void whenCommandHasCommentsThenNoIssueIsDetected() {
        // given
        final var workflowAsString = """
            # Few comments added
            commands:
            - class: sleep
              name: Sleep command
              inputs:
              - name: sleep.time
                value: 1000
            - class: print
              name: Print command
              inputs:
              - name: print.keys
                # Add more comments
                value:
                - sleep.time
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(0, validationMessages.size());
    }

    @Test
    public void whenCommandHasAliasesThenNoIssueIsDetected() {
        // given
        final var workflowAsString = """
            commands:
            - class: sleep
              name: Sleep command
              inputs:
              - name: &sleep_time sleep.time
                value: 1000
            - class: print
              name: Print command
              inputs:
              - name: print.keys
                value:
                - *sleep_time
            """;
        // when and then
        final var validationMessages = new WorkflowDefinitionValidatorService().validate(workflowAsString);
        assertEquals(0, validationMessages.size());
    }

}
