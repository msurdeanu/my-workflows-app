package org.myworkflows.serializer;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.BeanPropertyDefinition;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.networknt.schema.JsonSchema;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.List;

import static com.networknt.schema.JsonSchemaFactory.getInstance;
import static com.networknt.schema.SpecVersionDetector.detect;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class JsonFactory {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    static {
        MAPPER.registerModule(new JavaTimeModule());

        MAPPER.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        MAPPER.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
    }

    public static <T> T fromJsonToObject(String content, Class<T> clazz) {
        try {
            return MAPPER.readValue(content, clazz);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    public static JsonSchema fromJsonToSchema(JsonNode jsonNode) {
        return getInstance(detect(jsonNode)).getSchema(jsonNode);
    }

    public static String toString(Object object, String defaultValue) {
        try {
            return MAPPER.writeValueAsString(object);
        } catch (IOException exception) {
            return defaultValue;
        }
    }

    public static String toPrettyString(Object object, String defaultValue) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(object);
        } catch (IOException exception) {
            return defaultValue;
        }
    }

    public static List<BeanPropertyDefinition> findJsonProperties(Class<?> clazz) {
        final var userType = MAPPER.getTypeFactory().constructType(clazz);
        final var introspection = MAPPER.getSerializationConfig().introspect(userType);
        return introspection.findProperties();
    }

}
