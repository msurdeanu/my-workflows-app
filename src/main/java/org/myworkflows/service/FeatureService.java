package org.myworkflows.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.domain.Feature;
import org.myworkflows.domain.UserAccountDetails;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.util.SecurityUtil;
import org.springframework.stereotype.Service;

/**
 * Whether a {@link Feature} is turned on. The answer is read from the settings, which are held in memory, so it costs
 * nothing to ask on every call: a change takes effect at once, without a restart.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class FeatureService {

    private final SettingProvider settingProvider;

    public boolean isEnabled(Feature feature) {
        return Boolean.TRUE.equals(settingProvider.getOrDefault(feature.getSettingKey(), feature.isEnabledByDefault()));
    }

    public void setEnabled(Feature feature, boolean enabled) {
        settingProvider.set(feature.getSettingKey(), enabled);
        log.info("Feature {} was turned {} by {}.", feature, enabled ? "on" : "off",
            SecurityUtil.findCurrentAccount().map(UserAccountDetails::getUsername).orElse("the application"));
    }

}
