package org.myworkflows.domain;

import org.myworkflows.exception.WorkflowRuntimeException;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serial;
import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class WorkflowRunCache implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Map<String, Object> cachedObjectMap = new HashMap<>();

    private boolean cacheObjectMapComplete = true;

    @SuppressWarnings("checkstyle:LineLength")
    public <T> T get(String key, Class<T> clazz) {
        return find(key, clazz)
            .orElseThrow(() -> new WorkflowRuntimeException("No value found for key '" + key + "' or value type is not assignable from class '" + clazz.getName() + "'"));
    }

    @SuppressWarnings({"unchecked", "checkstyle:LineLength"})
    public <T> List<T> getList(String key, Class<T> clazz) {
        return findList(key, clazz)
            .orElseThrow(() -> new WorkflowRuntimeException("No value found for key '" + key + "' or value type is not list or value type is not assignable from class '" + clazz.getName() + "' or value is empty list"));
    }

    public Optional<Object> find(String key) {
        return ofNullable(cachedObjectMap.get(key));
    }

    public <T> Optional<T> find(String key, Class<T> clazz) {
        return find(key)
            .filter(value -> clazz.isAssignableFrom(value.getClass()))
            .map(clazz::cast);
    }

    @SuppressWarnings("unchecked")
    public <T> Optional<List<T>> findList(String key, Class<T> clazz) {
        return find(key)
            .filter(value -> value instanceof List)
            .map(List.class::cast)
            .filter(list -> !list.isEmpty())
            .filter(list -> clazz.isAssignableFrom(list.get(0).getClass()))
            .map(list -> (List<T>) list);
    }

    public <T> T lookup(String key, Class<T> clazz, boolean mandatory) {
        return mandatory ? get(key, clazz) : find(key, clazz).orElse(null);
    }

    public <T> List<T> lookupList(String key, Class<T> clazz, boolean mandatory) {
        return mandatory ? getList(key, clazz) : findList(key, clazz).orElse(null);
    }

    public Object put(String key, Object value) {
        return cachedObjectMap.put(key, value);
    }

    public Object remove(String key) {
        return cachedObjectMap.remove(key);
    }

    private void writeObject(ObjectOutputStream objectOutputStream) throws IOException {
        final var serializedObjectMap = getSerializedObjectMap();
        if (serializedObjectMap.size() != cachedObjectMap.size()) {
            cacheObjectMapComplete = false;
        }

        objectOutputStream.defaultWriteObject();
        objectOutputStream.writeBoolean(cacheObjectMapComplete);
        objectOutputStream.writeInt(serializedObjectMap.size());
        for (var entry : serializedObjectMap.entrySet()) {
            objectOutputStream.writeObject(entry.getKey());
            objectOutputStream.writeObject(entry.getValue());
        }
    }

    private void readObject(ObjectInputStream objectInputStream) throws IOException, ClassNotFoundException {
        objectInputStream.defaultReadObject();
        cacheObjectMapComplete = objectInputStream.readBoolean();
        final var size = objectInputStream.readInt();
        cachedObjectMap = new HashMap<>(size);
        for (int i = 0; i < size; i++) {
            final var key = (String) objectInputStream.readObject();
            final var value = (Serializable) objectInputStream.readObject();
            cachedObjectMap.put(key, value);
        }
    }

    private Map<String, Serializable> getSerializedObjectMap() {
        final var objectMap = new HashMap<String, Serializable>();
        cachedObjectMap.forEach((key, value) -> {
            if (value instanceof Serializable serializedValue) {
                objectMap.put(key, serializedValue);
            }
        });
        return objectMap;
    }

}