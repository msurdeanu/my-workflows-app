package org.myworkflows.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.Feature;
import org.myworkflows.domain.event.EditorTipOnSubmitEvent;
import org.myworkflows.provider.SettingProvider;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
@ExtendWith(MockitoExtension.class)
public final class EditorTipSchedulerServiceTest {

    @Mock
    private SettingProvider settingProvider;

    @Mock
    private FeatureService featureService;

    @Mock
    private EventBroadcaster eventBroadcaster;

    @Test
    public void whenTipsAreTurnedOffThenNoTipIsSent() {
        // given
        when(featureService.isEnabled(Feature.EDITOR_TIPS)).thenReturn(false);

        // when
        new EditorTipSchedulerService(settingProvider, featureService, eventBroadcaster).run();

        // then
        verify(eventBroadcaster, never()).broadcast(any());
    }

    @Test
    public void whenTipsAreTurnedOnThenATipIsSent() {
        // given
        when(featureService.isEnabled(Feature.EDITOR_TIPS)).thenReturn(true);
        when(settingProvider.getOrDefault(eq("tipsCount"), anyInt())).thenReturn(23);

        // when
        new EditorTipSchedulerService(settingProvider, featureService, eventBroadcaster).run();

        // then
        verify(eventBroadcaster).broadcast(any(EditorTipOnSubmitEvent.class));
    }

}
