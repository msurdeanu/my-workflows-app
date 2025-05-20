package org.myworkflows.view.component;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.Html;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.shared.Registration;
import org.myworkflows.config.BaseConfig;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public abstract class ResponsiveLayout extends FlexLayout implements HasResizeableWidth {

    private Registration registration;

    public ResponsiveLayout() {
        setId("responsiveLayout");
        setSizeFull();
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        final var page = attachEvent.getUI().getPage();
        registration = page.addBrowserWindowResizeListener(event -> adjustByWidth(event.getWidth()));
        page.retrieveExtendedClientDetails(event -> adjustByWidth(event.getBodyClientWidth()));

    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        registration.remove();
        super.onDetach(detachEvent);
    }

    protected Component createHeader(String subTitleOnLeft, Component... componentsOnRight) {
        final var headerLayout = new FlexLayout();
        headerLayout.setWidthFull();
        headerLayout.setFlexDirection(FlexDirection.ROW);

        final var leftHeaderDiv = new Div();
        leftHeaderDiv.addClassName("header-left");
        final var pageSubTitle = new H3(subTitleOnLeft);
        leftHeaderDiv.add(pageSubTitle);
        headerLayout.add(leftHeaderDiv);

        final var rightHeaderDiv = new Div();
        rightHeaderDiv.addClassName("header-right");
        final var horizontalLayout = new HorizontalLayout(componentsOnRight);
        horizontalLayout.setWidthFull();
        horizontalLayout.setJustifyContentMode(JustifyContentMode.END);
        horizontalLayout.setSpacing(true);
        rightHeaderDiv.add(horizontalLayout);
        headerLayout.add(rightHeaderDiv);

        return headerLayout;
    }

    protected Component createContent(Component... components) {
        final var contentDiv = new Div();
        contentDiv.addClassName("content");
        contentDiv.add(components);
        return contentDiv;
    }

    protected Component createFooter(BaseConfig baseConfig) {
        final var footerDiv = new Div();
        footerDiv.addClassName("footer");
        footerDiv.add(new Html(getTranslation("footer.copyright", baseConfig.getUrl(), baseConfig.getName())));
        return footerDiv;
    }

}
