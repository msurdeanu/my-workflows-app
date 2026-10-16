package org.myworkflows.config;

import com.vaadin.flow.spring.security.VaadinAwareSecurityContextHolderStrategyConfiguration;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.UserRole;
import org.myworkflows.restapi.filter.UserTokenFilter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Configuration("securityApiConfig")
@RequiredArgsConstructor
@Import(VaadinAwareSecurityContextHolderStrategyConfiguration.class)
@ConditionalOnProperty(name = "my-workflows.config.feature.restApiEnabled", havingValue = "true")
public class SecurityApiConfig {

    private static final String ADMIN = UserRole.ADMIN.getLabel();

    @Order(0)
    @Bean
    public SecurityFilterChain securityFilterApi(HttpSecurity http, AuthenticationProvider authenticationProvider,
                                                 UserTokenFilter userTokenFilter) throws Exception {
        final var httpSecurity = http
            .securityMatcher("/api/**")
            // A call is authenticated by the token it carries, not by a session cookie, so there is nothing a
            // forged request could ride on - while the CSRF check would reject every POST of an API client.
            .csrf(AbstractHttpConfigurer::disable)
            .sessionManagement(item -> item.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            // What only an admin may do on a page, only an admin may do here: change a definition or a template, and
            // anything about the parameters and the placeholders, whose pages nobody else opens. Reading and running
            // is left to every user, and so is cancelling, deleting and replaying a run, like on the runs page.
            .authorizeHttpRequests(auth -> auth
                .requestMatchers("/api/v1/workflow-parameters/**", "/api/v1/workflow-placeholders/**").hasAuthority(ADMIN)
                .requestMatchers(HttpMethod.POST, "/api/v1/workflow-definitions", "/api/v1/workflow-templates",
                    "/api/v1/workflow-definitions/*/duplicate", "/api/v1/workflow-templates/*/duplicate").hasAuthority(ADMIN)
                .requestMatchers(HttpMethod.PUT, "/api/v1/workflow-definitions/*", "/api/v1/workflow-templates/*").hasAuthority(ADMIN)
                .requestMatchers(HttpMethod.DELETE, "/api/v1/workflow-definitions/*", "/api/v1/workflow-templates/*").hasAuthority(ADMIN)
                .requestMatchers("/api/v1/**").authenticated());

        http.authenticationProvider(authenticationProvider);
        http.addFilterBefore(userTokenFilter, UsernamePasswordAuthenticationFilter.class);

        return httpSecurity.build();
    }

    @Bean
    public AuthenticationProvider authenticationProvider(UserDetailsService userDetailsService, PasswordEncoder passwordEncoder) {
        final var authProvider = new DaoAuthenticationProvider(userDetailsService);
        authProvider.setPasswordEncoder(passwordEncoder);
        return authProvider;
    }

}

