package org.myworkflows.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Mihai Surdeanu
 * @since 2.3
 */
@Setter
@Getter
@Configuration("reportConfig")
@ConfigurationProperties(prefix = "my-workflows.config.report")
public class ReportConfig {

    /**
     * Where the reports are written, a directory per run.
     */
    private String baseDirectory = "reports";

}
