package org.myworkflows.provider;

import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.myworkflows.domain.Setting;
import org.myworkflows.domain.SettingType;
import org.myworkflows.repository.SettingRepository;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Optional.of;
import static java.util.Optional.ofNullable;
import static org.myworkflows.exception.WorkflowRuntimeException.wrap;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
@Slf4j
public final class DatabaseSettingProvider implements InvocationHandler {

    private static final Map<SettingType, Function<String, Optional<Object>>> SETTING_TO_OBJECT_MAPPING = createMapper();

    private final Lock lock = new ReentrantLock();

    private final DefaultSettingProvider defaultSettingProvider;

    private final SettingRepository settingRepository;

    private final List<Setting> availableSettings;

    public DatabaseSettingProvider(@NonNull DefaultSettingProvider defaultSettingProvider,
                                   @NonNull SettingRepository settingRepository) {
        this.defaultSettingProvider = defaultSettingProvider;
        this.settingRepository = settingRepository;
        this.availableSettings = settingRepository.findAllByOrderByPosition().stream()
            .map(setting -> Pair.of(setting, transformTo(setting.getType(), setting.getValue())))
            .filter(pair -> pair.getRight().isPresent())
            .map(pair -> {
                pair.getLeft().setComputedValue(pair.getRight().get());
                return pair.getLeft();
            })
            .collect(Collectors.toList());
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (isGetOrDefault(method)) {
            return getSetting(method, args);
        } else if (isGetAll(method)) {
            return List.copyOf(availableSettings);
        } else if (isSet(method)) {
            return setSetting(args);
        } else {
            return method.invoke(defaultSettingProvider, args);
        }
    }

    private static Map<SettingType, Function<String, Optional<Object>>> createMapper() {
        Map<SettingType, Function<String, Optional<Object>>> mapping = new EnumMap<>(SettingType.class);
        mapping.put(SettingType.STR, Optional::of);
        mapping.put(SettingType.STR_H, Optional::of);
        mapping.put(SettingType.TEXT, Optional::of);
        mapping.put(SettingType.TEXT_H, Optional::of);
        mapping.put(SettingType.PASSWORD, Optional::of);
        mapping.put(SettingType.INTEGER, value -> of(Integer.parseInt(value)));
        mapping.put(SettingType.INTEGER_H, value -> of(Integer.parseInt(value)));
        mapping.put(SettingType.BOOLEAN, value -> of(Boolean.parseBoolean(value)));
        mapping.put(SettingType.BOOLEAN_H, value -> of(Boolean.parseBoolean(value)));
        return mapping;
    }

    private Optional<Object> transformTo(SettingType type, String value) {
        return ofNullable(SETTING_TO_OBJECT_MAPPING.get(type))
            .flatMap(function -> function.apply(value));
    }

    private boolean isGetOrDefault(Method method) {
        return "getOrDefault".equals(method.getName());
    }

    private boolean isGetAll(Method method) {
        return "getAll".equals(method.getName());
    }

    private boolean isSet(Method method) {
        return "set".equals(method.getName());
    }

    private Object getSetting(Method method, Object[] args) throws Throwable {
        if (args.length < 1 || !(args[0] instanceof String)) {
            return method.invoke(defaultSettingProvider, args);
        }

        return availableSettings.stream()
            .filter(setting -> setting.getKey().equals(args[0]))
            .findFirst()
            .map(Setting::getComputedValue)
            .orElseGet(() -> wrap(() -> method.invoke(defaultSettingProvider, args)));
    }

    private Object setSetting(Object[] args) {
        if (args.length != 2 || !(args[0] instanceof String)) {
            return null;
        }

        lock.lock();
        try {
            availableSettings.stream()
                .filter(setting -> setting.getKey().equals(args[0]))
                .filter(setting -> !setting.getComputedValue().equals(args[1]))
                .findFirst()
                .ifPresent(setting -> setSettingValue(setting, args[1]));
        } finally {
            lock.unlock();
        }

        return null;
    }

    private void setSettingValue(Setting setting, Object value) {
        setting.setComputedValue(value);
        setting.setValue(value + StringUtils.EMPTY);
        settingRepository.save(setting);
    }

}
