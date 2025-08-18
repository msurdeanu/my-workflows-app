package org.myworkflows.provider;

import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.Setting;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
@Slf4j
public final class DefaultSettingProvider implements SettingProvider {

    @Override
    public List<Setting> getAll() {
        return List.of();
    }

    @Override
    public <T> T getOrDefault(String key, T defaultValue) {
        log.warn("Default value for setting key '{}' is provided.", key);
        return defaultValue;
    }

    @Override
    public <T> void set(String key, T toValue) {
        // Nothing to do
    }

}
