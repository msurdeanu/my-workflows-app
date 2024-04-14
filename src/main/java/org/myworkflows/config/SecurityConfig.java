package org.myworkflows.config;

import com.vaadin.flow.spring.security.VaadinWebSecurity;
import lombok.RequiredArgsConstructor;
import org.myworkflows.ApplicationManager;
import org.myworkflows.service.ApplicationUserDetailsService;
import org.myworkflows.view.LoginView;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Configuration("securityConfig")
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig extends VaadinWebSecurity {

    public static final String REMEMBER_ME = "remember-me";

    private final ApplicationManager applicationManager;

    @Override
    public void configure(final HttpSecurity http) throws Exception {
        http.rememberMe(config -> config.alwaysRemember(true).key(REMEMBER_ME).rememberMeCookieName(REMEMBER_ME));
        http.authorizeHttpRequests(config -> config.requestMatchers(new AntPathRequestMatcher("/logo.png")).permitAll());
        super.configure(http);

        setLoginView(http, LoginView.class);
    }

    @Override
    public void configure(final WebSecurity web) throws Exception {
        super.configure(web);
    }

    @Bean
    public UserDetailsService userDetailsService() {
        return new ApplicationUserDetailsService(applicationManager);
    }


    @Bean
    public BCryptPasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

}

