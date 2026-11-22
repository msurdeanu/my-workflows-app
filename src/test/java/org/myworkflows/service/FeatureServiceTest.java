package org.myworkflows.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.Feature;
import org.myworkflows.provider.SettingProvider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
@ExtendWith(MockitoExtension.class)
public final class FeatureServiceTest {

    @Mock
    private SettingProvider settingProvider;

    @Test
    public void whenSettingSaysSoThenTheFeatureIsOn() {
        // given
        when(settingProvider.getOrDefault("featureRestApi", false)).thenReturn(true);
        when(settingProvider.getOrDefault("featureEditorTips", true)).thenReturn(false);

        // when
        final var featureService = new FeatureService(settingProvider);

        // then
        assertTrue(featureService.isEnabled(Feature.REST_API));
        assertFalse(featureService.isEnabled(Feature.EDITOR_TIPS));
    }

    @Test
    public void whenFeatureIsTurnedOnThenItsSettingIsChanged() {
        // when
        new FeatureService(settingProvider).setEnabled(Feature.CLAUDE, true);

        // then
        verify(settingProvider).set("featureClaude", true);
    }

    @Test
    public void whenFeatureIsNamedThenItsTranslationsFollowItsName() {
        assertEquals("features.rest-api", Feature.REST_API.getTranslationPrefix());
        assertEquals("features.claude", Feature.CLAUDE.getTranslationPrefix());
        assertEquals("features.editor-tips", Feature.EDITOR_TIPS.getTranslationPrefix());
    }

}
