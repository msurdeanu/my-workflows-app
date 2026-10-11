package org.myworkflows.view.component;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.orderedlayout.FlexLayout;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.router.AfterNavigationEvent;
import com.vaadin.flow.router.AfterNavigationObserver;

import java.util.Map;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public abstract class ResponsiveLayout extends FlexLayout implements HasResizeableWidth, AfterNavigationObserver {

    private final BreakpointTracker breakpointTracker = new BreakpointTracker(this);

    private PageFilters pageFilters;
    private Map<String, String> pathFilterValues = Map.of();

    public ResponsiveLayout() {
        addClassName("responsive-layout");
        setSizeFull();
    }

    /**
     * The filters are restored once the navigation is over: the router records the new URL in the browser history
     * before this runs, so the URL rewritten here replaces that entry and never the one Back returns to.
     */
    @Override
    public void afterNavigation(AfterNavigationEvent afterNavigationEvent) {
        if (pageFilters != null) {
            pageFilters.restore(afterNavigationEvent.getLocation().getQueryParameters(), pathFilterValues);
            pathFilterValues = Map.of();
        }
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        breakpointTracker.attach(attachEvent.getUI());
    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        breakpointTracker.detach();
        super.onDetach(detachEvent);
    }

    protected Component createHeader(String subTitleOnLeft, Component... componentsOnRight) {
        final var headerLayout = new FlexLayout();
        headerLayout.addClassName("header");
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
        horizontalLayout.addClassName("header-filters");
        horizontalLayout.setWidthFull();
        horizontalLayout.setJustifyContentMode(JustifyContentMode.END);
        horizontalLayout.setSpacing(true);
        rightHeaderDiv.add(horizontalLayout);
        headerLayout.add(rightHeaderDiv);

        return headerLayout;
    }

    /**
     * Keeps the given filters of the page in its URL and in the session (see {@link PageFilters}).
     *
     * @return the row of chips of the filters in use, meant to go between the header and the content.
     */
    protected Component createFilters(String route, PageFilter... filters) {
        pageFilters = new PageFilters(route, filters);
        return pageFilters;
    }

    /**
     * Called from {@code setParameter} by a page whose path can carry a filter, like {@code workflow/templates/42}:
     * the values given here win over the query parameters and the session for this navigation only.
     */
    protected void setPathFilterValues(Map<String, String> values) {
        pathFilterValues = values;
    }

    protected Component createContent(Component... components) {
        final var contentDiv = new Div();
        contentDiv.addClassName("content");
        contentDiv.add(components);
        return contentDiv;
    }

    /**
     * @param page the prefix of the translations of the guide, see {@link PageGuide}.
     * @return the guide of the page, meant to go after the content: the content then ends where its grid ends, for the
     *     guide to follow the grid instead of sitting at the bottom of the screen.
     */
    protected PageGuide createGuide(String page) {
        addClassName("with-guide");
        return new PageGuide(page);
    }

    /**
     * @return the guide of a page whose content fills the screen instead of ending with a grid, meant to go after the
     *     content: it is a bar under the content, which keeps a height worth working in once the guide is opened.
     */
    protected PageGuide createFoldedGuide(String page) {
        addClassName("with-folded-guide");
        return new PageGuide(page);
    }

}
