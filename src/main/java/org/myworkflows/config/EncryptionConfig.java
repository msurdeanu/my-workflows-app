package org.myworkflows.config;

import org.myworkflows.holder.EncryptionHolder;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Configuration("encryptionConfig")
@ConfigurationProperties(prefix = "my-workflows.config.encryption")
public class EncryptionConfig {

    public void setAlgorithm(String algorithm) {
        EncryptionHolder.INSTANCE.setAlgorithm(algorithm);
    }

    public void setSecretKey(String secretKey) {
        EncryptionHolder.INSTANCE.setSecretKey(secretKey);
    }

}
