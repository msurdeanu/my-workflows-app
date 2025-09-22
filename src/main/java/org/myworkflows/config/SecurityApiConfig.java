package org.myworkflows.config;

import com.vaadin.flow.spring.security.VaadinAwareSecurityContextHolderStrategyConfiguration;
import lombok.RequiredArgsConstructor;
import org.myworkflows.restapi.filter.UserTokenFilter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
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

    @Order(0)
    @Bean
    public SecurityFilterChain securityFilterApi(HttpSecurity http, AuthenticationProvider authenticationProvider,
                                                 UserTokenFilter userTokenFilter) throws Exception {
        final var httpSecurity = http
            .securityMatcher("/api/**")
            .sessionManagement(item -> item.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            .authorizeHttpRequests(auth -> auth
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

