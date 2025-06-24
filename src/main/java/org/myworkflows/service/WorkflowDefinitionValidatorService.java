package org.myworkflows.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.networknt.schema.JsonSchema;
import com.networknt.schema.ValidationMessage;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.util.Set;

import static org.myworkflows.serializer.SerializerFactory.toObject;
import static org.myworkflows.serializer.SerializerFactory.toSchema;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowDefinitionValidatorService {

    private static final JsonSchema WORKFLOW_SCHEMA;

    static {
        final var classPathResource = new ClassPathResource("workflow_schema.json");
        try (var inputStream = classPathResource.getInputStream()) {
            WORKFLOW_SCHEMA = toSchema(toObject(new String(inputStream.readAllBytes(), StandardCharsets.UTF_8), JsonNode.class));
            WORKFLOW_SCHEMA.initializeValidators();
        } catch (Exception exception) {
            throw new WorkflowRuntimeException("An exception occurred during process of reading workflow schema from classpath.", exception);
        }
    }

    public Set<ValidationMessage> validate(String wokflowAsString) {
        return WORKFLOW_SCHEMA.validate(toObject(wokflowAsString, JsonNode.class));
    }

}
