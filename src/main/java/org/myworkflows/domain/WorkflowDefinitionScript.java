package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.domain.command.AbstractCommand;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class WorkflowDefinitionScript {

    @JsonProperty("commands")
    private List<AbstractCommand> commands;

    @JsonProperty("finallyCommands")
    private List<AbstractCommand> finallyCommands = List.of();

    public static WorkflowDefinitionScript of(List<WorkflowDefinitionScript> workflowDefinitionScripts) {
        final var workflowDefinitionScript = new WorkflowDefinitionScript();
        workflowDefinitionScript.setCommands(workflowDefinitionScripts.stream()
            .flatMap(item -> item.getCommands().stream())
            .collect(Collectors.toList()));
        workflowDefinitionScript.setFinallyCommands(workflowDefinitionScripts.stream()
            .flatMap(item -> item.getFinallyCommands().stream())
            .collect(Collectors.toList()));
        return workflowDefinitionScript;
    }

}
