package org.myworkflows.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.networknt.schema.Error;
import com.networknt.schema.InputFormat;
import com.networknt.schema.Schema;
import com.networknt.schema.SchemaRegistry;
import com.networknt.schema.SchemaRegistryConfig;
import com.networknt.schema.SpecificationVersion;
import com.networknt.schema.path.PathType;
import org.myworkflows.domain.command.CommandType;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.CommandTypeHolder;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
public final class WorkflowDefinitionValidatorService {

    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();

    // The schema names the command types, so it is made again once a library brings or takes away one of them.
    private static final Supplier<Schema> WORKFLOW_SCHEMA = CommandTypeHolder.INSTANCE.derive(WorkflowDefinitionValidatorService::createSchema);

    public List<Error> validate(String wokflowAsString) {
        return WORKFLOW_SCHEMA.get().validate(wokflowAsString, InputFormat.YAML);
    }

    private static Schema createSchema(Collection<CommandType> commandTypes) {
        final var classPathResource = new ClassPathResource("workflow_schema.json");
        try (var inputStream = classPathResource.getInputStream()) {
            final var schemaNode = JSON_MAPPER.readTree(inputStream);
            addCommandTypes((ObjectNode) schemaNode.at("/$defs/command"), commandTypes);
            // The default dialect is used only as a fallback: the "$schema" keyword of the schema takes precedence.
            // The legacy path type keeps the "$.commands[0]" notation in messages instead of the JSON Pointer one.
            final var schema = SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12,
                    builder -> builder.schemaRegistryConfig(SchemaRegistryConfig.builder().pathType(PathType.LEGACY).build()))
                .getSchema(new ByteArrayInputStream(JSON_MAPPER.writeValueAsBytes(schemaNode)), InputFormat.JSON);
            schema.initializeValidators();
            return schema;
        } catch (Exception exception) {
            throw new WorkflowRuntimeException("An exception occurred during process of reading workflow schema from classpath.", exception);
        }
    }

    /**
     * Completes the schema of a command with what only the command types can tell: the values its {@code class} field
     * accepts, and which of them cannot go without {@code subcommands}.
     */
    private static void addCommandTypes(ObjectNode commandNode, Collection<CommandType> commandTypes) {
        final var classNames = ((ObjectNode) commandNode.at("/properties/class")).putArray("enum");
        final var conditions = commandNode.putArray("allOf");
        commandTypes.forEach(commandType -> {
            classNames.add(commandType.name());
            if (commandType.hasSubcommands()) {
                final var condition = conditions.addObject();
                condition.putObject("if").putObject("properties").putObject("class").put("const", commandType.name());
                condition.putObject("then").putArray("required").add("subcommands");
            }
        });
    }

}
