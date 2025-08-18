package org.myworkflows.provider;

import org.myworkflows.domain.Setting;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
public interface SettingProvider {

    List<Setting> getAll();

    <T> T getOrDefault(String key, T defaultValue);

    <T> void set(String key, T toValue);

}
