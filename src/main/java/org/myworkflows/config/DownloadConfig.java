package org.myworkflows.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Setter
@Getter
@Configuration("downloadConfig")
@ConfigurationProperties(prefix = "my-workflows.config.download")
public class DownloadConfig {

    /**
     * Where the files a run offers to download are kept, a directory per run.
     */
    private String baseDirectory = "downloads";

    /**
     * The directories of this server a download command may read from, with whatever they hold. None means anywhere:
     * whoever writes a workflow can read any file with a groovy command anyway, but a list keeps a mistake from offering
     * a file which nobody meant to share. A file of another server, read over SSH, is not concerned.
     */
    private List<String> allowedDirectories = new ArrayList<>();

}
