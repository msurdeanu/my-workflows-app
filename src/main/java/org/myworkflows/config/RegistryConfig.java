package org.myworkflows.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.myworkflows.domain.User;

import java.util.List;

@Configuration
@ConfigurationProperties(prefix = "registry")
@Getter
@Setter
public class RegistryConfig {

    private List<User> users;

    private List<User> admins;

}
