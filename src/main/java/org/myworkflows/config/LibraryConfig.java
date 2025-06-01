package org.myworkflows.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Mihai Surdeanu
 * @since 1.2.0
 */
@Setter
@Getter
@Configuration("libraryConfig")
@ConfigurationProperties(prefix = "my-workflows.config.library")
public class LibraryConfig {

    public static final String JAR_EXTENSION = ".jar";

    private String baseDirectory;
    private boolean reloadAfterUpload = false;

}
