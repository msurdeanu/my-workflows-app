package org.myworkflows.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Setter
@Getter
@Configuration("baseConfig")
@ConfigurationProperties(prefix = "my-workflows.config.base")
public class BaseConfig {

    private String logoSrc = "src/main/resources/META-INF/resources/logo.png";
    private String name = "My Workflows";
    private String url = "https://myworkflows.org";
    private String version = "1.0";

    private int tipsCount = 23;
    private int tipsFrequencyInSeconds = 15;

    private String rememberMeCookieName = "mw-rm";
    private short rememberMeCookieDays = 30;

}
