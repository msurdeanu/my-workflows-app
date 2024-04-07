package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.domain.command.AbstractCommand;

import java.util.List;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class WorkflowDefinition {

    @JsonIgnore
    private final UUID id;

    @JsonProperty("name")
    private String name;

    @JsonProperty("commands")
    private List<AbstractCommand> commands;

    @JsonProperty("finallyCommands")
    private List<AbstractCommand> finallyCommands = List.of();

    public WorkflowDefinition() {
        id = UUID.randomUUID();
    }

}
