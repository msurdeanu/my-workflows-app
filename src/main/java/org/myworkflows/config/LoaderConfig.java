package org.myworkflows.config;

import lombok.Getter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Getter
@Configuration("loaderConfig")
@ConfigurationProperties(prefix = "my-workflows.config.loader")
public class LoaderConfig {

    private final List<String> jars = List.of();

}
