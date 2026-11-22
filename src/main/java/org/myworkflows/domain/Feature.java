package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Locale;

/**
 * A part of the application which an admin turns on or off on the features page, while it runs. Each one is a hidden
 * setting ({@code bool_h}) of its own, which the settings page does not show; the first time version 2.5 started, it
 * took the value of its {@code my-workflows.config.feature.*} property, if there was one (see {@code V13}).
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Getter
@RequiredArgsConstructor
public enum Feature {

    /**
     * The calls to {@code /api/v1}, authenticated by the API tokens of the users.
     */
    REST_API("featureRestApi", false),

    /**
     * Everything which calls Claude: one switch for all of it, as each of them sends what is masked the same way.
     */
    CLAUDE("featureClaude", false),

    /**
     * A tip about the editor, shown on the development page every few seconds.
     */
    EDITOR_TIPS("featureEditorTips", true);

    private final String settingKey;

    /**
     * What the feature is when its setting cannot be read.
     */
    private final boolean enabledByDefault;

    /**
     * @return the start of the translations of the feature: {@code <prefix>.name}, {@code <prefix>.description}...
     */
    public String getTranslationPrefix() {
        return "features." + name().toLowerCase(Locale.ROOT).replace('_', '-');
    }

}
