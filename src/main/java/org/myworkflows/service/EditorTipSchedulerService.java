package org.myworkflows.service;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.Feature;
import org.myworkflows.domain.event.EditorTipOnSubmitEvent;
import org.myworkflows.provider.SettingProvider;
import org.springframework.stereotype.Service;

import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Sends a tip about the editor to the development pages every few seconds. It is scheduled whether the tips are turned
 * on or not and sends nothing while they are off, so that the features page turns them on and off while it runs.
 *
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public final class EditorTipSchedulerService implements Runnable {

    private static final ScheduledExecutorService SCHEDULER = Executors.newSingleThreadScheduledExecutor();
    private static final Random RANDOM = new Random();
    private static final int DEFAULT_TIPS_COUNT = 23;
    private static final int DEFAULT_TIPS_FREQUENCY = 15;

    private final SettingProvider settingProvider;
    private final FeatureService featureService;
    private final EventBroadcaster eventBroadcaster;

    @PostConstruct
    public void init() {
        // scheduleAtFixedRate rejects a period of zero or less, which would abort the application startup.
        final var frequency = Math.max(1, settingProvider.getOrDefault("tipsFrequency", DEFAULT_TIPS_FREQUENCY));
        SCHEDULER.scheduleAtFixedRate(this, 0, frequency, TimeUnit.SECONDS);
    }

    @Override
    public void run() {
        // scheduleAtFixedRate cancels the task for good as soon as it throws, so nothing may escape here.
        try {
            if (!featureService.isEnabled(Feature.EDITOR_TIPS)) {
                return;
            }
            final var tipsCount = settingProvider.getOrDefault("tipsCount", DEFAULT_TIPS_COUNT);
            if (tipsCount < 1) {
                return;
            }
            eventBroadcaster.broadcast(EditorTipOnSubmitEvent.builder().tipId(RANDOM.nextInt(tipsCount)).build());
        } catch (Exception exception) {
            log.warn("Editor tip could not be broadcast.", exception);
        }
    }

}
