package org.myworkflows.config;

import org.myworkflows.holder.file.FileSourceHolder;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Configuration("fileSourceConfig")
@ConfigurationProperties(prefix = "my-workflows.config.file-source")
public class FileSourceConfig {

    public void setBaseDirectory(String baseDirectory) {
        FileSourceHolder.INSTANCE.setBaseDirectory(baseDirectory);
    }

}
