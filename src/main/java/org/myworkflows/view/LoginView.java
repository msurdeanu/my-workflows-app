package org.myworkflows.view;

import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.login.LoginForm;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.BeforeEnterObserver;
import com.vaadin.flow.router.Route;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Route(value = LoginView.ROUTE)
public class LoginView extends Composite<VerticalLayout> implements BeforeEnterObserver {

    public static final String ROUTE = "login";

    private final LoginForm loginForm = new LoginForm();

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();

        layout.setSizeFull();
        layout.setAlignItems(FlexComponent.Alignment.CENTER);
        layout.setJustifyContentMode(FlexComponent.JustifyContentMode.CENTER);

        loginForm.setForgotPasswordButtonVisible(false);
        loginForm.setAction("login");
        layout.add(loginForm);

        return layout;
    }

    @Override
    public void beforeEnter(BeforeEnterEvent beforeEnterEvent) {
        if (beforeEnterEvent.getLocation().getQueryParameters().getParameters().containsKey("error")) {
            loginForm.setError(true);
        }
    }

}

