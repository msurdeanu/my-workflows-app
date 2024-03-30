package org.myworkflows;

import com.vaadin.flow.component.page.AppShellConfigurator;
import com.vaadin.flow.component.page.Push;
import com.vaadin.flow.shared.communication.PushMode;
import com.vaadin.flow.theme.Theme;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.event.Event;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.event.EventListener;
import org.springframework.core.annotation.Order;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@SpringBootApplication
@RequiredArgsConstructor
@Push(PushMode.AUTOMATIC)
@Theme(value = "simple")
public class Application extends SpringBootServletInitializer implements AppShellConfigurator {
    private final ApplicationManager applicationManager;

    public static void main(final String[] args) {
        SpringApplication.run(Application.class, args);
    }

    @EventListener
    @Order(1)
    public void registerAllListeners(final ApplicationReadyEvent event) {
        event.getApplicationContext()
                .getBeansOfType(org.myworkflows.domain.event.EventListener.class)
                .forEach(this::registerListener);
    }

    private void registerListener(final String key, final org.myworkflows.domain.event.EventListener<Event> value) {
        applicationManager.getBeanOfType(EventBroadcaster.class).register(value::onEventReceived, value.getEventType());
    }

}
