package org.myworkflows.config;

import com.vaadin.flow.spring.security.VaadinWebSecurity;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.myworkflows.view.LoginView;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

import java.util.List;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Configuration("securityConfig")
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig extends VaadinWebSecurity {

    public static final String USER = "USER";

    public static final String ADMIN = "ADMIN";

    private final RegistryConfig registryConfig;

    @Override
    @SuppressWarnings("removal")
    public void configure(HttpSecurity http) throws Exception {
        http.authorizeHttpRequests().requestMatchers(new AntPathRequestMatcher("/logo.png")).permitAll();
        super.configure(http);

        setLoginView(http, LoginView.class);
    }

    @Override
    public void configure(WebSecurity web) throws Exception {
        super.configure(web);
    }

    @Bean
    public UserDetailsService userDetailsService() {
        final var manager = new InMemoryUserDetailsManager();
        ofNullable(registryConfig.getUsers())
            .orElse(List.of())
            .stream()
            .map(user -> org.springframework.security.core.userdetails.User
                .withUsername(user.getUsername())
                .password(user.getPassword())
                .roles(USER).build())
            .forEach(manager::createUser);
        ofNullable(registryConfig.getAdmins())
            .orElse(List.of())
            .stream()
            .map(user -> org.springframework.security.core.userdetails.User
                .withUsername(user.getUsername())
                .password(user.getPassword())
                .roles(USER, ADMIN).build())
            .forEach(manager::createUser);
        return manager;
    }

    @Bean
    public BCryptPasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

}

