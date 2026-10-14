package org.myworkflows.serializer;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.deser.DeserializationProblemHandler;
import com.fasterxml.jackson.databind.jsontype.TypeIdResolver;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.UnknownCommand;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.CommandTypeHolder;

import java.io.IOException;
import java.util.function.Supplier;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class SerializerFactory {

    // A mapper remembers the class it found behind the "class" field of a command, so it is made again once the command
    // types change: a library which is loaded later, or loaded again, is then taken into account.
    private static final Supplier<YAMLMapper> MAPPER = CommandTypeHolder.INSTANCE.derive(_ -> createMapper());

    private static final Supplier<YAMLMapper> MAPPER_KEEPING_UNKNOWN_COMMANDS = CommandTypeHolder.INSTANCE.derive(_ -> {
        final var mapper = createMapper();
        mapper.addHandler(new UnknownCommandHandler());
        return mapper;
    });

    private static final DefaultPrettyPrinter PRETTY_PRINTER = new DefaultPrettyPrinter();

    public static <T> T toObject(String content, Class<T> clazz) {
        return WorkflowRuntimeException.wrap(() -> MAPPER.get().readValue(content, clazz));
    }

    /**
     * Reads something which was saved earlier, when the command types may not have been the ones of today: a command
     * whose type is not known is kept as an {@link UnknownCommand} instead of making the whole content unreadable.
     */
    public static <T> T toObjectKeepingUnknownCommands(String content, Class<T> clazz) {
        return WorkflowRuntimeException.wrap(() -> MAPPER_KEEPING_UNKNOWN_COMMANDS.get().readValue(content, clazz));
    }

    public static String toString(Object object, String defaultValue) {
        try {
            return MAPPER.get().writeValueAsString(object);
        } catch (IOException exception) {
            return defaultValue;
        }
    }

    public static String toPrettyString(Object object, String defaultValue) {
        try {
            Object newObject = object;
            if (object instanceof String objectAsString) {
                newObject = MAPPER.get().readTree(objectAsString);
            }
            return MAPPER.get().writer(PRETTY_PRINTER).writeValueAsString(newObject);
        } catch (IOException exception) {
            return defaultValue;
        }
    }

    private static YAMLMapper createMapper() {
        final var mapper = new YAMLMapper();
        mapper.registerModule(new JavaTimeModule());

        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapper.disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER);
        mapper.disable(YAMLGenerator.Feature.USE_NATIVE_TYPE_ID);
        mapper.enable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        return mapper;
    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    private static final class UnknownCommandHandler extends DeserializationProblemHandler {

        @Override
        public JavaType handleUnknownTypeId(DeserializationContext context, JavaType baseType, String subTypeId, TypeIdResolver idResolver,
                                            String failureMessage) {
            return baseType.hasRawClass(AbstractCommand.class) ? context.constructType(UnknownCommand.class) : null;
        }

    }

}
