package org.myworkflows.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Setter
@Getter
@Configuration("featureConfig")
@ConfigurationProperties(prefix = "my-workflows.config.feature")
public class FeatureConfig {

    private boolean restApiEnabled = false;
    private boolean editorTipSchedulerEnabled = true;

}
