package org.myworkflows.serializer;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.command.AbstractCommand;
import org.myworkflows.domain.command.UnknownCommand;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.holder.CommandTypeHolder;
import tools.jackson.core.JacksonException;
import tools.jackson.databind.DeserializationContext;
import tools.jackson.databind.JavaType;
import tools.jackson.databind.cfg.DateTimeFeature;
import tools.jackson.databind.deser.DeserializationProblemHandler;
import tools.jackson.databind.jsontype.TypeIdResolver;
import tools.jackson.dataformat.yaml.YAMLMapper;
import tools.jackson.dataformat.yaml.YAMLWriteFeature;

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
    private static final Supplier<YAMLMapper> MAPPER = CommandTypeHolder.INSTANCE.derive(_ -> createMapper().build());

    private static final Supplier<YAMLMapper> MAPPER_KEEPING_UNKNOWN_COMMANDS = CommandTypeHolder.INSTANCE.derive(_ -> createMapper()
        .addHandler(new UnknownCommandHandler())
        .build());

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
        } catch (JacksonException exception) {
            return defaultValue;
        }
    }

    public static String toPrettyString(Object object, String defaultValue) {
        try {
            Object newObject = object;
            if (object instanceof String objectAsString) {
                newObject = MAPPER.get().readTree(objectAsString);
            }
            return MAPPER.get().writerWithDefaultPrettyPrinter().writeValueAsString(newObject);
        } catch (JacksonException exception) {
            return defaultValue;
        }
    }

    /**
     * @return a mapper which reads and writes as the one of Jackson 2 did - the fields of a command in the order of
     *     their declaration, among others -, so that a script is written as it always was.
     */
    private static YAMLMapper.Builder createMapper() {
        return YAMLMapper.builder()
            .configureForJackson2()
            .changeDefaultPropertyInclusion(inclusion -> inclusion.withValueInclusion(JsonInclude.Include.NON_NULL))
            .disable(YAMLWriteFeature.WRITE_DOC_START_MARKER)
            .disable(YAMLWriteFeature.USE_NATIVE_TYPE_ID)
            .enable(YAMLWriteFeature.MINIMIZE_QUOTES)
            .disable(DateTimeFeature.WRITE_DATES_AS_TIMESTAMPS);
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
