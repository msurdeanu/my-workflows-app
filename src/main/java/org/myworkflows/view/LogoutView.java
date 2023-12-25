package org.myworkflows.view;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.BeforeEnterObserver;
import com.vaadin.flow.router.Route;
import org.myworkflows.service.SecurityService;

import jakarta.annotation.security.PermitAll;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@PermitAll
@Route(value = LogoutView.ROUTE)
public class LogoutView extends Div implements BeforeEnterObserver {

    public static final String ROUTE = "logout";

    private final SecurityService securityService;

    public LogoutView(final SecurityService securityService) {
        this.securityService = securityService;
    }

    @Override
    public void beforeEnter(final BeforeEnterEvent beforeEnterEvent) {
        securityService.logout();
    }

}
