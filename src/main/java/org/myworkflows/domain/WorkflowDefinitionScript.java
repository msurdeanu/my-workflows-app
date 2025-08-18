package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.domain.command.AbstractCommand;

import java.util.List;

import static java.util.stream.Collectors.toList;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class WorkflowDefinitionScript {

    @JsonProperty("commands")
    private List<AbstractCommand> commands;

    @JsonProperty("finallyCommands")
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private List<AbstractCommand> finallyCommands = List.of();

    public static WorkflowDefinitionScript empty() {
        return of(List.of(), List.of());
    }

    public static WorkflowDefinitionScript of(List<AbstractCommand> commands, List<AbstractCommand> finallyCommands) {
        final var workflowDefinitionScript = new WorkflowDefinitionScript();
        workflowDefinitionScript.setCommands(commands);
        workflowDefinitionScript.setFinallyCommands(finallyCommands);
        return workflowDefinitionScript;
    }

    public static WorkflowDefinitionScript of(List<WorkflowDefinitionScript> workflowDefinitionScripts) {
        final var workflowDefinitionScript = new WorkflowDefinitionScript();
        workflowDefinitionScript.setCommands(workflowDefinitionScripts.stream()
            .flatMap(item -> item.getCommands().stream())
            .collect(toList()));
        workflowDefinitionScript.setFinallyCommands(workflowDefinitionScripts.stream()
            .flatMap(item -> item.getFinallyCommands().stream())
            .collect(toList()));
        return workflowDefinitionScript;
    }

}
