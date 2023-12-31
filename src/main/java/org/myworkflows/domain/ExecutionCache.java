package org.myworkflows.domain;

import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ExecutionCache {

    private final Map<String, Object> cachedObjectMap = new HashMap<>();

    @SuppressWarnings("checkstyle:LineLength")
    public <T> T get(final String key, final Class<T> clazz) {
        return find(key, clazz)
            .orElseThrow(() -> new WorkflowRuntimeException("No value found for key '" + key + "' or value type is not assignable from class '" + clazz.getName() + "'"));
    }

    @SuppressWarnings({"unchecked", "checkstyle:LineLength"})
    public <T> List<T> getList(final String key, final Class<T> clazz) {
        return findList(key, clazz)
            .orElseThrow(() -> new WorkflowRuntimeException("No value found for key '" + key + "' or value type is not list or value type is not assignable from class '" + clazz.getName() + "' or value is empty list"));
    }

    public Optional<Object> find(final String key) {
        return ofNullable(cachedObjectMap.get(key));
    }

    public <T> Optional<T> find(final String key, final Class<T> clazz) {
        return find(key)
            .filter(value -> clazz.isAssignableFrom(value.getClass()))
            .map(clazz::cast);
    }

    @SuppressWarnings("unchecked")
    public <T> Optional<List<T>> findList(final String key, final Class<T> clazz) {
        return find(key)
            .filter(value -> value instanceof List)
            .map(List.class::cast)
            .filter(list -> !list.isEmpty())
            .filter(list -> clazz.isAssignableFrom(list.get(0).getClass()))
            .map(list -> (List<T>) list);
    }

    public <T> T lookup(final String key, final Class<T> clazz, final boolean mandatory) {
        return mandatory ? get(key, clazz) : find(key, clazz).orElse(null);
    }

    public <T> List<T> lookupList(final String key, final Class<T> clazz, final boolean mandatory) {
        return mandatory ? getList(key, clazz) : findList(key, clazz).orElse(null);
    }

    public Object put(final String key, final Object value) {
        return cachedObjectMap.put(key, value);
    }

    public Object remove(final String key) {
        return cachedObjectMap.remove(key);
    }

}
