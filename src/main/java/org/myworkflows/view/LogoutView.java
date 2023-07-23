package org.myworkflows.view;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.BeforeEnterObserver;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.VaadinService;
import org.springframework.security.core.context.SecurityContextHolder;

import jakarta.annotation.security.PermitAll;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@PermitAll
@Route(value = LogoutView.ROUTE)
public class LogoutView extends Div implements BeforeEnterObserver {

    public static final String ROUTE = "logout";

    @Override
    public void beforeEnter(BeforeEnterEvent beforeEnterEvent) {
        SecurityContextHolder.clearContext();
        VaadinService.getCurrentRequest().getWrappedSession().invalidate();
    }

}
