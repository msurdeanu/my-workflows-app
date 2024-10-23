package org.myworkflows.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.networknt.schema.JsonSchema;
import com.networknt.schema.ValidationMessage;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Set;

import static org.myworkflows.serializer.JsonFactory.fromJsonToObject;
import static org.myworkflows.serializer.JsonFactory.fromJsonToSchema;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Service
public final class WorkflowDefinitionValidatorService {

    private static final JsonSchema WORKFLOW_SCHEMA;

    static {
        final var classPathResource = new ClassPathResource("workflow_schema.json");
        final var stringBuilder = new StringBuilder();
        try (var inputStreamReader = new InputStreamReader(classPathResource.getInputStream());
             var buffer = new BufferedReader(inputStreamReader)) {
            String line;
            while ((line = buffer.readLine()) != null) {
                stringBuilder.append(line);
            }
        } catch (Exception exception) {
            throw new WorkflowRuntimeException("An exception occurred during process of reading workflow schema from classpath.", exception);
        }

        WORKFLOW_SCHEMA = fromJsonToSchema(fromJsonToObject(stringBuilder.toString(), JsonNode.class));
        WORKFLOW_SCHEMA.initializeValidators();
    }

    public Set<ValidationMessage> validate(String wokflowAsString) {
        return WORKFLOW_SCHEMA.validate(fromJsonToObject(wokflowAsString, JsonNode.class));
    }

}
