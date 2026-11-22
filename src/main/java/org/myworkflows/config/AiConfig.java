package org.myworkflows.config;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.Feature;
import org.myworkflows.domain.WorkflowParameter;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.WorkflowPlaceholder;
import org.myworkflows.holder.AiClientHolder;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.service.FeatureService;
import org.myworkflows.service.WorkflowParameterService;
import org.myworkflows.service.WorkflowPlaceholderService;
import org.myworkflows.util.SecretMasker;
import org.springframework.context.annotation.Configuration;

import java.util.List;
import java.util.stream.Stream;

/**
 * Tells {@link AiClientHolder} where to read what it needs on every call, as a command which calls Claude cannot reach a
 * bean: whether an admin turned Claude on, the model of the settings, and the secrets to mask in what is sent - the
 * values of the parameters of type {@code PASS}, and of the parameters and placeholders whose name says
 * {@code password}.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Configuration("aiConfig")
@RequiredArgsConstructor
public class AiConfig {

    public static final String MODEL_SETTING = "aiModel";

    private final FeatureService featureService;
    private final SettingProvider settingProvider;
    private final WorkflowParameterService workflowParameterService;
    private final WorkflowPlaceholderService workflowPlaceholderService;

    @PostConstruct
    public void init() {
        AiClientHolder.INSTANCE.configure(
            () -> featureService.isEnabled(Feature.CLAUDE),
            () -> settingProvider.getOrDefault(MODEL_SETTING, AiClientHolder.DEFAULT_MODEL),
            this::findSecretValues,
            AiClientHolder::createFromEnvironment);
    }

    @PreDestroy
    public void destroy() {
        AiClientHolder.INSTANCE.close();
    }

    List<String> findSecretValues() {
        return Stream.concat(
                workflowParameterService.getAllItems().filter(AiConfig::isSecret).map(WorkflowParameter::getValue),
                workflowPlaceholderService.getAllItems().filter(placeholder -> SecretMasker.isSecretName(placeholder.getName()))
                    .map(WorkflowPlaceholder::getValue))
            .toList();
    }

    private static boolean isSecret(WorkflowParameter parameter) {
        return parameter.getType() == WorkflowParameterType.PASS || SecretMasker.isSecretName(parameter.getName());
    }

}
