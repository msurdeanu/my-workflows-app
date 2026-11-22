package org.myworkflows.config;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.myworkflows.domain.Feature;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.domain.ai.AiStatus;
import org.myworkflows.holder.AiClientHolder;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.service.FeatureService;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.service.WorkflowPlaceholderService;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
@ExtendWith(MockitoExtension.class)
public final class AiConfigTest {

    @Mock
    private FeatureService featureService;

    @Mock
    private SettingProvider settingProvider;

    @Mock
    private WorkflowParameterService workflowParameterService;

    @Mock
    private WorkflowPlaceholderService workflowPlaceholderService;

    @AfterEach
    public void tearDown() {
        AiClientHolder.INSTANCE.configure(() -> false, () -> AiClientHolder.DEFAULT_MODEL, List::of, Optional::empty);
    }

    @Test
    public void whenSecretsAreAskedForThenThePasswordsOfParametersAndPlaceholdersAreGiven() {
        // given
        when(workflowParameterService.getAllItems()).thenAnswer(_ -> Stream.of(
            WorkflowParameter.of("vault.token", WorkflowParameterType.PASS, "token-value"),
            WorkflowParameter.of("db.password", WorkflowParameterType.STR, "db-value"),
            WorkflowParameter.of("region", WorkflowParameterType.STR, "eu-west")));
        when(workflowPlaceholderService.getAllItems()).thenAnswer(_ -> Stream.of(
            WorkflowPlaceholder.of("SMTP_PASSWORD", "smtp-value"),
            WorkflowPlaceholder.of("SMTP_HOST", "mail.local")));

        // when
        final var secrets = newAiConfig().findSecretValues();

        // then
        assertEquals(List.of("token-value", "db-value", "smtp-value"), secrets);
    }

    @Test
    public void whenApplicationStartsThenTheHolderReadsTheFeatureAndTheModelOfTheSettings() {
        // given
        when(featureService.isEnabled(Feature.CLAUDE)).thenReturn(false);
        when(settingProvider.getOrDefault(AiConfig.MODEL_SETTING, AiClientHolder.DEFAULT_MODEL)).thenReturn("claude-sonnet-5-5");

        // when
        newAiConfig().init();

        // then
        assertEquals(AiStatus.DISABLED, AiClientHolder.INSTANCE.getStatus());
        assertEquals("claude-sonnet-5-5", AiClientHolder.INSTANCE.getDefaultModel());
    }

    private AiConfig newAiConfig() {
        return new AiConfig(featureService, settingProvider, workflowParameterService, workflowPlaceholderService);
    }

}
