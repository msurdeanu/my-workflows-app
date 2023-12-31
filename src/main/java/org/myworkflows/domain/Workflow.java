package org.myworkflows.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import org.myworkflows.domain.command.AbstractCommand;

import java.util.List;
import java.util.Set;
import java.util.UUID;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@JsonIgnoreProperties(ignoreUnknown = true)
public class Workflow {

    @JsonIgnore
    private final String id;

    @JsonProperty("name")
    private String name;

    @JsonProperty("tags")
    private Set<String> tags = Set.of();

    @JsonProperty("commands")
    private List<AbstractCommand> commands;

    @JsonProperty("finallyCommands")
    private List<AbstractCommand> finallyCommands = List.of();

    public Workflow() {
        id = UUID.randomUUID().toString();
    }

}
