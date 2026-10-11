package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.NativeButton;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.router.Location;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.server.VaadinSession;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static java.util.Optional.ofNullable;

/**
 * The filters of a page, kept in two places. In the URL, as query parameters, so that Back returns to the page as it
 * was filtered and a filtered page can be bookmarked or shared. In the session, so that coming back to the page from
 * the menu finds the filters as they were left. Shows a removable chip per filter in use and a button resetting all.
 *
 * <p>The URL is only ever replaced, never pushed: a filter typed letter by letter must not add a step to Back.</p>
 *
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class PageFilters extends Div {

    private static final String SESSION_KEY_PREFIX = PageFilters.class.getName() + ".";

    private final String route;
    private final List<PageFilter> filters;

    private boolean restoring;

    public PageFilters(String route, PageFilter... filters) {
        this.route = route;
        this.filters = List.of(filters);
        addClassName("page-filters");
        setVisible(false);
        this.filters.forEach(filter -> ofNullable(filter.source()).ifPresent(source -> source.addValueChangeListener(_ -> {
            if (!restoring) {
                save();
            }
        })));
    }

    /**
     * Applies the values a path parameter stands for when there are any, otherwise the query parameters of the URL when
     * it names any of the filters, otherwise the values left in the session; a filter given no value gets its default.
     */
    public void restore(QueryParameters query, Map<String, String> pathValues) {
        final Map<String, String> values;
        if (!pathValues.isEmpty()) {
            values = pathValues;
        } else if (filters.stream().anyMatch(filter -> query.getParameters().containsKey(filter.name()))) {
            values = new LinkedHashMap<>();
            query.getParameters().forEach((name, list) -> list.stream().findFirst().ifPresent(value -> values.put(name, value)));
        } else {
            values = storedValues();
        }
        apply(values);
    }

    private void apply(Map<String, String> values) {
        restoring = true;
        try {
            filters.forEach(filter -> filter.setter().accept(values.getOrDefault(filter.name(), "")));
        } finally {
            restoring = false;
        }
        save();
    }

    private void save() {
        final var values = new LinkedHashMap<String, String>();
        filters.forEach(filter -> {
            final var value = filter.getter().get();
            if (!value.isEmpty()) {
                values.put(filter.name(), value);
            }
        });
        VaadinSession.getCurrent().setAttribute(SESSION_KEY_PREFIX + route, values);
        UI.getCurrent().getPage().getHistory().replaceState(null, new Location(route, QueryParameters.simple(values)));
        renderChips(values);
    }

    @SuppressWarnings("unchecked")
    private Map<String, String> storedValues() {
        return ofNullable(VaadinSession.getCurrent().getAttribute(SESSION_KEY_PREFIX + route))
            .map(value -> (Map<String, String>) value)
            .orElse(Map.of());
    }

    private void renderChips(Map<String, String> values) {
        removeAll();
        filters.forEach(filter -> ofNullable(values.get(filter.name()))
            .map(filter.describer())
            .ifPresent(text -> add(createChip(filter, text))));
        if (getComponentCount() > 0) {
            final var resetButton = new Button(getTranslation("filters.reset"), _ -> apply(Map.of()));
            resetButton.addThemeVariants(ButtonVariant.TERTIARY, ButtonVariant.SMALL);
            add(resetButton);
        }
        setVisible(getComponentCount() > 0);
    }

    private Component createChip(PageFilter filter, String text) {
        final var removeButton = new NativeButton();
        removeButton.add(VaadinIcon.CLOSE.create());
        removeButton.addClassName("page-filter-remove");
        removeButton.getElement().setAttribute("aria-label", getTranslation("filters.remove", text));
        removeButton.getElement().setAttribute("title", getTranslation("filters.remove", text));
        removeButton.addClickListener(_ -> {
            final var values = new LinkedHashMap<String, String>();
            filters.forEach(other -> values.put(other.name(), other == filter ? "" : other.getter().get()));
            apply(values);
        });
        final var chip = new Span(new Span(text), removeButton);
        chip.addClassName("page-filter");
        return chip;
    }

}
