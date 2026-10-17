package org.myworkflows.view.component;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.dialog.DialogVariant;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.NativeButton;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.dom.Element;
import com.vaadin.flow.router.QueryParameters;
import com.vaadin.flow.router.RouterLink;
import org.myworkflows.domain.search.GlobalSearchResult;
import org.myworkflows.service.GlobalSearchService;
import org.myworkflows.view.DocPageView;
import org.myworkflows.view.WorkflowDevelopmentView;
import org.myworkflows.view.WorkflowParameterView;
import org.myworkflows.view.WorkflowPlaceholderView;
import org.myworkflows.view.WorkflowTemplateView;

import java.util.Locale;

import static java.util.Optional.ofNullable;

/**
 * The search of the header, reachable from every page: a button shaped like a search field - a bare magnifier on a
 * phone - opens a dialog listing what holds the text while typing (see {@link GlobalSearchService} for what is searched
 * and for whom). Enter opens the first result, the arrow keys walk through them. On a keyboard, "/" and Ctrl+K (Cmd+K on
 * a Mac) open it from anywhere but the script editor, which keeps its own Ctrl+K.
 *
 * @author Mihai Surdeanu
 * @since 2.3
 */
public final class GlobalSearch extends NativeButton {

    private static final int MAX_RESULTS = 10;
    private static final int SEARCH_DELAY_MILLIS = 150;
    private static final String DIALOG_WIDTH = "min(600px, 94vw)";

    /**
     * Installed once per page on the document, so the shortcut works whatever has the focus. "/" is left alone
     * while typing in a field, Ctrl+K is left to Ace, where it finds the next match; both simply click the button, which
     * keeps the dialog opening in one place.
     */
    private static final String SHORTCUT_SCRIPT = """
        const trigger = this;
        if (/Mac|iPhone|iPad/.test(navigator.platform)) {
            trigger.querySelector('kbd').textContent = '\\u2318K';
        }
        if (window.__globalSearchKeydown) {
            document.removeEventListener('keydown', window.__globalSearchKeydown);
        }
        window.__globalSearchKeydown = event => {
            const path = event.composedPath();
            const target = path[0];
            const typing = target.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(target.tagName);
            const inEditor = path.some(element => element.classList && element.classList.contains('ace_editor'));
            const key = (event.key || '').toLowerCase();
            const ctrlK = key === 'k' && (event.ctrlKey || event.metaKey) && !event.altKey && !event.shiftKey && !inEditor;
            const slash = key === '/' && !event.ctrlKey && !event.metaKey && !event.altKey && !typing;
            if ((ctrlK || slash) && trigger.isConnected) {
                event.preventDefault();
                if (!event.repeat) {
                    trigger.click();
                }
            }
        };
        document.addEventListener('keydown', window.__globalSearchKeydown);""";

    /**
     * iOS opens the keyboard only for a focus given while the tap is handled, and the field is focused later, once the
     * server has opened the dialog. So a touch focuses a hidden input on the spot, which brings the keyboard up; the
     * focus then moves on to the field, and iOS keeps the keyboard open when the focus goes from one input to another.
     * The hidden input leaves as soon as it loses the focus, or after a while should the dialog never open.
     */
    private static final String TOUCH_FOCUS_SCRIPT = """
        const trigger = this;
        if (trigger.__touchFocus) {
            return;
        }
        trigger.__touchFocus = true;
        trigger.addEventListener('click', () => {
            if (!matchMedia('(pointer: coarse)').matches) {
                return;
            }
            const proxy = document.createElement('input');
            proxy.tabIndex = -1;
            proxy.setAttribute('aria-label', trigger.getAttribute('aria-label'));
            proxy.style.cssText = 'position:fixed;top:0;left:0;width:1px;height:1px;padding:0;border:0;opacity:0;font-size:16px';
            proxy.addEventListener('blur', () => proxy.remove(), { once: true });
            document.body.appendChild(proxy);
            proxy.focus({ preventScroll: true });
            setTimeout(() => proxy.blur(), 5000);
        });""";

    /**
     * The arrow keys move the focus between the field and the results on the client, without a server round-trip.
     */
    private static final String ARROW_KEYS_SCRIPT = """
        this.addEventListener('keydown', event => {
            if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp') {
                return;
            }
            const results = [...this.querySelectorAll('.global-search-result')];
            if (results.length === 0) {
                return;
            }
            event.preventDefault();
            const index = results.indexOf(event.target.closest('.global-search-result'));
            const next = event.key === 'ArrowDown' ? Math.min(index + 1, results.length - 1) : index - 1;
            if (next < 0) {
                this.querySelector('.global-search-field').focus();
            } else {
                results[next].focus();
            }
        });""";

    private final Dialog dialog = new Dialog();
    private final TextField searchField = new TextField();
    private final Div results = new Div();

    private final GlobalSearchService globalSearchService;

    public GlobalSearch(GlobalSearchService globalSearchService) {
        this.globalSearchService = globalSearchService;

        final var shortcut = new Element("kbd");
        shortcut.setText(getTranslation("search.trigger.shortcut"));
        add(VaadinIcon.SEARCH.create(), new Span(getTranslation("search.trigger")));
        getElement().appendChild(shortcut);
        getElement().setAttribute("type", "button");
        addClassName("global-search-trigger");
        getElement().setAttribute("title", getTranslation("search.trigger.tooltip"));
        getElement().setAttribute("aria-label", getTranslation("search.trigger"));
        getElement().setAttribute("aria-keyshortcuts", "/ Control+K Meta+K");
        addClickListener(_ -> open());

        createDialog();
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        getElement().executeJs(SHORTCUT_SCRIPT);
        getElement().executeJs(TOUCH_FOCUS_SCRIPT);
    }

    private void createDialog() {
        dialog.addClassName("global-search-dialog");
        dialog.addThemeVariants(DialogVariant.NO_PADDING);
        dialog.getElement().setAttribute("aria-label", getTranslation("search.trigger"));
        dialog.setWidth(DIALOG_WIDTH);
        // anchored near the top rather than centered, so the field stays put while the results below it change;
        // a dialog given a position is no longer centered by the overlay, so the left edge is computed as well
        dialog.setTop("10vh");
        dialog.setLeft("calc((100% - " + DIALOG_WIDTH + ") / 2)");
        dialog.addOpenedChangeListener(event -> {
            if (!event.isOpened()) {
                searchField.clear();
            }
        });

        searchField.addClassName("global-search-field");
        searchField.setPlaceholder(getTranslation("search.field.placeholder"));
        searchField.setPrefixComponent(VaadinIcon.SEARCH.create());
        searchField.setClearButtonVisible(true);
        searchField.setValueChangeMode(ValueChangeMode.LAZY);
        searchField.setValueChangeTimeout(SEARCH_DELAY_MILLIS);
        searchField.addValueChangeListener(event -> showResults(event.getValue()));
        searchField.getElement().setAttribute("autocomplete", "off");
        searchField.getElement().setAttribute("autocapitalize", "off");
        searchField.getElement().setAttribute("spellcheck", "false");
        // a phone keyboard labels its Enter key "Go", which is what Enter does here
        searchField.getElement().setAttribute("enterkeyhint", "go");
        // the value travels with the key press, as the lazy value change may not have reached the server yet
        searchField.getElement().addEventListener("keydown", event -> openFirstMatch(event.getEventData().get("element.value").asString()))
            .setFilter("event.key === 'Enter' && !event.isComposing")
            .addEventData("element.value");

        results.addClassName("global-search-results");

        final var keys = new Span(getTranslation("search.keys"));
        keys.addClassName("global-search-keys");

        final var panel = new Div(searchField, results, keys);
        panel.addClassName("global-search-panel");
        // the dialog leaves the page when it closes and comes back as new elements, so the listener goes on every attach
        panel.addAttachListener(_ -> panel.getElement().executeJs(ARROW_KEYS_SCRIPT));
        dialog.add(panel);
    }

    private void open() {
        if (dialog.isOpened()) {
            return;
        }

        showResults(searchField.getValue());
        dialog.open();
        searchField.focus();
    }

    private void showResults(String text) {
        results.removeAll();
        if (text.isBlank()) {
            results.add(createMessage(getTranslation("search.hint")));
            return;
        }

        final var matches = globalSearchService.search(text, MAX_RESULTS);
        if (matches.isEmpty()) {
            results.add(createMessage(getTranslation("search.empty", text.strip())));
        }
        matches.forEach(result -> results.add(createResult(result)));
    }

    private void openFirstMatch(String text) {
        globalSearchService.search(text, 1).stream().findFirst().ifPresent(result -> {
            dialog.close();
            navigate(UI.getCurrent(), result);
        });
    }

    /**
     * Where a result is opened: a definition in the development page, which shows its script - read-only for a user who
     * is no admin -, a documentation page or the command reference on its tab, and the rest on its own page, filtered.
     * {@link #createLink} goes to the same places.
     */
    private static void navigate(UI ui, GlobalSearchResult result) {
        switch (result.kind()) {
            case DEFINITION -> ui.navigate(WorkflowDevelopmentView.class, result.id());
            case TEMPLATE -> ui.navigate(WorkflowTemplateView.class, result.id());
            case DOC_PAGE, COMMAND -> ui.navigate(DocPageView.class, docPageOf(result));
            default -> ui.navigate(filteredViewOf(result.kind()), byName(result));
        }
    }

    private Component createResult(GlobalSearchResult result) {
        final var kind = result.kind().name().toLowerCase(Locale.ROOT).replace('_', '-');
        final var name = new Span(result.name());
        name.addClassName("global-search-name");
        final var details = new Span(new Span(getTranslation("search.kind." + kind)));
        details.addClassName("global-search-details");
        ofNullable(result.excerpt()).ifPresent(excerpt -> {
            final var excerptSpan = new Span(excerpt);
            excerptSpan.addClassName("global-search-excerpt");
            details.add(excerptSpan);
        });
        final var text = new Span(name, details);
        text.addClassName("global-search-text");

        final var link = createLink(result);
        link.add(iconOf(result.kind()).create(), text);
        link.addClassName("global-search-result");
        link.getElement().addEventListener("click", _ -> dialog.close());
        return link;
    }

    private static RouterLink createLink(GlobalSearchResult result) {
        final var link = new RouterLink();
        switch (result.kind()) {
            case DEFINITION -> link.setRoute(WorkflowDevelopmentView.class, result.id());
            case TEMPLATE -> link.setRoute(WorkflowTemplateView.class, result.id());
            case DOC_PAGE, COMMAND -> link.setRoute(DocPageView.class, docPageOf(result));
            default -> {
                link.setRoute(filteredViewOf(result.kind()));
                link.setQueryParameters(byName(result));
            }
        }
        return link;
    }

    private static String docPageOf(GlobalSearchResult result) {
        return result.kind() == GlobalSearchResult.Kind.COMMAND ? DocPageView.COMMAND_REFERENCE : result.name();
    }

    /**
     * The page of a kind which is opened by its name filter.
     */
    private static Class<? extends Component> filteredViewOf(GlobalSearchResult.Kind kind) {
        return switch (kind) {
            case PARAMETER -> WorkflowParameterView.class;
            case PLACEHOLDER -> WorkflowPlaceholderView.class;
            default -> throw new IllegalArgumentException("No page is filtered by the name of a " + kind + ".");
        };
    }

    /**
     * The name filter of the page, which matches a part of the name: the page may list a few more than the one found.
     */
    private static QueryParameters byName(GlobalSearchResult result) {
        return QueryParameters.of("name", result.name());
    }

    private static VaadinIcon iconOf(GlobalSearchResult.Kind kind) {
        return switch (kind) {
            case DEFINITION -> VaadinIcon.CUBE;
            case TEMPLATE -> VaadinIcon.CUBES;
            case DOC_PAGE -> VaadinIcon.BOOK;
            case COMMAND -> VaadinIcon.TERMINAL;
            case PARAMETER -> VaadinIcon.OPTIONS;
            case PLACEHOLDER -> VaadinIcon.ARCHIVES;
        };
    }

    private static Component createMessage(String text) {
        final var message = new Div(text);
        message.addClassName("global-search-message");
        return message;
    }

}
