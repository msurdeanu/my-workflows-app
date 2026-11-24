package org.myworkflows.domain.command;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.myworkflows.holder.CommandTypeHolder;
import tools.jackson.databind.DatabindContext;
import tools.jackson.databind.JavaType;
import tools.jackson.databind.jsontype.impl.TypeIdResolverBase;

import static java.util.stream.Collectors.joining;

/**
 * Tells which class stands behind the {@code class} field of a command, and the other way around, from the command
 * types the application knows at that moment - the ones brought by a library included.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class CommandTypeIdResolver extends TypeIdResolverBase {

    @Override
    public String idFromValue(DatabindContext context, Object value) {
        return CommandType.nameOf((AbstractCommand) value);
    }

    @Override
    public String idFromValueAndType(DatabindContext context, Object value, Class<?> suggestedType) {
        return idFromValue(context, value);
    }

    @Override
    public JavaType typeFromId(DatabindContext context, String id) {
        return CommandTypeHolder.INSTANCE.find(id)
            .map(commandType -> context.constructType(commandType.type()))
            .orElse(null);
    }

    @Override
    public String getDescForKnownTypeIds() {
        return CommandTypeHolder.INSTANCE.getAll().stream()
            .map(CommandType::name)
            .collect(joining(", ", "[", "]"));
    }

    @Override
    public JsonTypeInfo.Id getMechanism() {
        return JsonTypeInfo.Id.CUSTOM;
    }

}
