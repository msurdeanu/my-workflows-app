package org.myworkflows.service;

import com.networknt.schema.Error;
import com.networknt.schema.InputFormat;
import com.networknt.schema.Schema;
import com.networknt.schema.SchemaRegistry;
import com.networknt.schema.SchemaRegistryConfig;
import com.networknt.schema.SpecificationVersion;
import com.networknt.schema.path.PathType;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
public final class WorkflowDefinitionValidatorService {

    private static final Schema WORKFLOW_SCHEMA;

    static {
        final var classPathResource = new ClassPathResource("workflow_schema.json");
        try (var inputStream = classPathResource.getInputStream()) {
            // The default dialect is used only as a fallback: the "$schema" keyword of the schema takes precedence.
            // The legacy path type keeps the "$.commands[0]" notation in messages instead of the JSON Pointer one.
            WORKFLOW_SCHEMA = SchemaRegistry.withDefaultDialect(SpecificationVersion.DRAFT_2020_12,
                    builder -> builder.schemaRegistryConfig(SchemaRegistryConfig.builder().pathType(PathType.LEGACY).build()))
                .getSchema(inputStream, InputFormat.JSON);
            WORKFLOW_SCHEMA.initializeValidators();
        } catch (Exception exception) {
            throw new WorkflowRuntimeException("An exception occurred during process of reading workflow schema from classpath.", exception);
        }
    }

    public List<Error> validate(String wokflowAsString) {
        return WORKFLOW_SCHEMA.validate(wokflowAsString, InputFormat.YAML);
    }

}
