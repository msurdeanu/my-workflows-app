package org.myworkflows.config;

import com.vaadin.flow.spring.security.VaadinWebSecurity;
import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.holder.EncryptionHolder;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.restapi.filter.UserTokenFilter;
import org.myworkflows.service.ApplicationUserDetailsService;
import org.myworkflows.view.LoginView;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Configuration("securityConfig")
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig extends VaadinWebSecurity {

    private final ApplicationManager applicationManager;

    @Override
    public void configure(HttpSecurity http) throws Exception {
        final var settingProvider = applicationManager.getBeanOfType(SettingProvider.class);
        final var featureConfig = applicationManager.getBeanOfType(FeatureConfig.class);
        final var cookieName = settingProvider.getOrDefault("rememberMeCookieName", "mw-rm");

        http.rememberMe(
            config -> config.alwaysRemember(true)
                .key(cookieName)
                .tokenValiditySeconds(settingProvider.getOrDefault("rememberMeCookieDays", 30) * 86400)
                .rememberMeCookieName(cookieName));
        http.authorizeHttpRequests(config -> {
            config.requestMatchers("/logo.png").permitAll();
            if (featureConfig.isRestApiEnabled()) {
                config.requestMatchers("/api/**").permitAll();
            }
        });
        if (featureConfig.isRestApiEnabled()) {
            http.addFilterBefore(applicationManager.getBeanOfType(UserTokenFilter.class), UsernamePasswordAuthenticationFilter.class);
        }
        super.configure(http);

        setLoginView(http, LoginView.class);
    }

    @Override
    public void configure(WebSecurity web) throws Exception {
        super.configure(web);
    }

    @Bean
    public UserDetailsService userDetailsService() {
        return new ApplicationUserDetailsService(applicationManager);
    }


    @Bean
    public BCryptPasswordEncoder passwordEncoder(SettingProvider settingProvider) {
        EncryptionHolder.INSTANCE.setAlgorithm(settingProvider.getOrDefault("encryptionAlgo", "AES"));
        EncryptionHolder.INSTANCE.setSecretKey(settingProvider.getOrDefault("encryptionSecretKey", ""));

        return new BCryptPasswordEncoder();
    }

}

