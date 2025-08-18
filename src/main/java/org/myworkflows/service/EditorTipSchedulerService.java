package org.myworkflows.service;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.myworkflows.EventBroadcaster;
import org.myworkflows.domain.event.EditorTipOnSubmitEvent;
import org.myworkflows.provider.SettingProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Service
@RequiredArgsConstructor
@ConditionalOnProperty(name = "my-workflows.config.feature.editorTipSchedulerEnabled", havingValue = "true", matchIfMissing = true)
public final class EditorTipSchedulerService implements Runnable {

    private static final ScheduledExecutorService SCHEDULER = Executors.newSingleThreadScheduledExecutor();
    private static final Random RANDOM = new Random();

    private final SettingProvider settingProvider;
    private final EventBroadcaster eventBroadcaster;

    @PostConstruct
    public void init() {
        SCHEDULER.scheduleAtFixedRate(this, 0, settingProvider.getOrDefault("tipsFrequency", 15), TimeUnit.SECONDS);
    }

    @Override
    public void run() {
        eventBroadcaster.broadcast(EditorTipOnSubmitEvent.builder().tipId(RANDOM.nextInt(settingProvider.getOrDefault("tipsCount", 23))).build());
    }

}
