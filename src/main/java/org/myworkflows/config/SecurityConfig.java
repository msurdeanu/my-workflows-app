package org.myworkflows.config;

import com.vaadin.flow.spring.security.VaadinAwareSecurityContextHolderStrategyConfiguration;
import com.vaadin.flow.spring.security.VaadinSecurityConfigurer;
import org.myworkflows.ApplicationManager;
import org.myworkflows.holder.EncryptionHolder;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.service.ApplicationUserDetailsService;
import org.myworkflows.view.LoginView;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Configuration("securityConfig")
@EnableWebSecurity
@Import(VaadinAwareSecurityContextHolderStrategyConfiguration.class)
public class SecurityConfig {

    @Order(1)
    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http, SettingProvider settingProvider) throws Exception {
        final var cookieName = settingProvider.getOrDefault("rememberMeCookieName", "mw-rm");
        http.rememberMe(
            config -> config.alwaysRemember(true)
                .key(cookieName)
                .tokenValiditySeconds(settingProvider.getOrDefault("rememberMeCookieDays", 30) * 86400)
                .rememberMeCookieName(cookieName));
        return http.with(VaadinSecurityConfigurer.vaadin(), configurer -> configurer.loginView(LoginView.class)).build();
    }

    @Bean
    public UserDetailsService userDetailsService(ApplicationManager applicationManager) {
        return new ApplicationUserDetailsService(applicationManager);
    }

    @Bean
    public BCryptPasswordEncoder passwordEncoder(SettingProvider settingProvider) {
        EncryptionHolder.INSTANCE.setAlgorithm(settingProvider.getOrDefault("encryptionAlgo", "AES"));
        EncryptionHolder.INSTANCE.setSecretKey(settingProvider.getOrDefault("encryptionSecretKey", ""));

        return new BCryptPasswordEncoder();
    }

}

