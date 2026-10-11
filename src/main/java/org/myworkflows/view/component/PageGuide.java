package org.myworkflows.view.component;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.details.Details;
import com.vaadin.flow.component.details.DetailsVariant;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.page.WebStorage;
import org.myworkflows.domain.UserRole;

import static com.vaadin.flow.component.page.WebStorage.Storage.LOCAL_STORAGE;

/**
 * The guide under the grid of a page: what the page is for, a legend of what its grid shows, the steps to get
 * something done on it and a few things worth knowing. Every text is a translation found under {@code <page>.guide};
 * the texts - not the titles - are HTML, so that they can spell a key or a piece of code.
 *
 * <p>The guide starts folded, and the browser remembers, page by page, whether it was opened.</p>
 *
 * @author Mihai Surdeanu
 * @since 2.0
 */
public final class PageGuide extends Details {

    private static final String STORAGE_KEY_PREFIX = "page-guide.";

    private final Div body = new Div();

    private final String page;

    private String legendTitleKey = "page-guide.legend.title";
    private Div legend;
    private Div notes;

    public PageGuide(String page) {
        this.page = page;
        addClassName("page-guide");
        addThemeVariants(DetailsVariant.AURA_REVERSE);
        setSummary(createSummary());
        body.addClassName("page-guide-body");
        add(body);

        // shown once the browser has said whether it was opened, so that it never shows folded only to open
        setVisible(false);
        addOpenedChangeListener(event -> {
            if (event.isFromClient()) {
                WebStorage.setItem(LOCAL_STORAGE, STORAGE_KEY_PREFIX + page, String.valueOf(event.isOpened()));
            }
        });
    }

    /**
     * @return a button as the grids draw theirs, to be explained by {@link #legend(String, Component)}.
     */
    public static Button button(VaadinIcon icon, ButtonVariant... variants) {
        final var button = new Button(icon.create());
        button.addThemeVariants(ButtonVariant.SMALL);
        button.addThemeVariants(variants);
        return button;
    }

    /**
     * @return a badge as the grids draw theirs, to be explained by {@link #legend(String, Component)}.
     */
    public static Badge badge(String text, BadgeVariant... variants) {
        final var badge = new Badge(text);
        badge.addThemeVariants(BadgeVariant.SMALL);
        badge.addThemeVariants(variants);
        return badge;
    }

    /**
     * @return a link as the grids draw theirs, to be explained by {@link #legend(String, Component)}.
     */
    public static Anchor link(String text) {
        return new Anchor("#", text);
    }

    /**
     * @param valuesAndNames a value followed by what it stands for, as many times as there are values to spell.
     * @return the values next to each other, each above its name, to go into a {@link #note(String, Component...)}.
     */
    public static Component fields(String... valuesAndNames) {
        final var fields = new Div();
        fields.addClassName("page-guide-fields");
        for (int index = 0; index + 1 < valuesAndNames.length; index += 2) {
            final var value = new Span(valuesAndNames[index]);
            value.addClassName("page-guide-field-value");
            final var name = new Span(valuesAndNames[index + 1]);
            name.addClassName("page-guide-field-name");
            final var field = new Div(value, name);
            field.addClassName("page-guide-field");
            fields.add(field);
        }
        return fields;
    }

    /**
     * Names the legend of a page which shows something else than a grid. To be called before the first
     * {@link #legend(String, Component)}.
     */
    public PageGuide legendTitle(String titleKey) {
        legendTitleKey = titleKey;
        return this;
    }

    /**
     * Explains something the grid shows, next to a copy of it. Its title and its text are the translations
     * {@code <page>.guide.legend.<key>.title} and {@code <page>.guide.legend.<key>.text}.
     */
    public PageGuide legend(String key, Component sample) {
        return legend(key, sample, null);
    }

    /**
     * @param role the role somebody needs to use what is explained, named next to the title.
     */
    public PageGuide legend(String key, Component sample, UserRole role) {
        if (legend == null) {
            legend = createSection(legendTitleKey, "page-guide-legend");
        }
        // the copy only shows what to look for: it can be neither clicked nor reached with the keyboard
        final var sampleDiv = new Div(sample);
        sampleDiv.addClassName("page-guide-sample");
        sampleDiv.getElement().setAttribute("inert", true);
        sampleDiv.getElement().setAttribute("aria-hidden", "true");

        final var title = createTitle(getTranslation(page + ".guide.legend." + key + ".title"));
        if (role != null) {
            title.add(badge(getTranslation("page-guide.role." + role.name().toLowerCase()), BadgeVariant.CONTRAST));
        }
        legend.add(sampleDiv, new Div(title, createText(page + ".guide.legend." + key + ".text")));
        return this;
    }

    /**
     * Lists the steps of the page, numbered from 1: the translations {@code <page>.guide.steps.<number>.title}
     * and {@code <page>.guide.steps.<number>.text}, under the title {@code <page>.guide.steps.title}.
     */
    public PageGuide steps(int count) {
        final var steps = createSection(page + ".guide.steps.title", "page-guide-steps");
        for (int number = 1; number <= count; number++) {
            final var numberSpan = new Span(String.valueOf(number));
            numberSpan.addClassName("page-guide-step-number");
            numberSpan.getElement().setAttribute("aria-hidden", "true");
            steps.add(numberSpan, new Div(createTitle(getTranslation(page + ".guide.steps." + number + ".title")),
                createText(page + ".guide.steps." + number + ".text")));
        }
        return this;
    }

    /**
     * Adds something worth knowing: the translations {@code <page>.guide.notes.<key>.title} and
     * {@code <page>.guide.notes.<key>.text}.
     *
     * @param details what the text is about, drawn between the title and the text.
     */
    public PageGuide note(String key, Component... details) {
        if (notes == null) {
            notes = createSection("page-guide.notes.title", "page-guide-notes");
        }
        final var note = new Div(createTitle(getTranslation(page + ".guide.notes." + key + ".title")));
        note.addClassName("page-guide-note");
        note.add(details);
        note.add(createText(page + ".guide.notes." + key + ".text"));
        notes.add(note);
        return this;
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        WebStorage.getItem(attachEvent.getUI(), LOCAL_STORAGE, STORAGE_KEY_PREFIX + page, value -> {
            setOpened(Boolean.parseBoolean(value));
            setVisible(true);
        });
    }

    private Component createSummary() {
        final var icon = new Span(VaadinIcon.OPEN_BOOK.create());
        icon.addClassName("page-guide-icon");
        icon.getElement().setAttribute("aria-hidden", "true");
        final var title = new Span(getTranslation("page-guide.title"));
        title.addClassName("page-guide-title");
        final var intro = new Span(getTranslation(page + ".guide.intro"));
        intro.addClassName("page-guide-intro");
        final var texts = new Div(title, intro);
        texts.addClassName("page-guide-heading");
        final var summary = new Div(icon, texts);
        summary.addClassName("page-guide-summary");
        return summary;
    }

    /**
     * @return the list of the new section, which is added to the guide under its title.
     */
    private Div createSection(String titleKey, String listClassName) {
        final var list = new Div();
        list.addClassName(listClassName);
        final var section = new Div(new H4(getTranslation(titleKey)), list);
        section.addClassName("page-guide-section");
        body.add(section);
        return list;
    }

    private Div createTitle(String text) {
        final var title = new Div(new Span(text));
        title.addClassName("page-guide-item-title");
        return title;
    }

    private Div createText(String key) {
        final var text = new Div();
        text.addClassName("page-guide-text");
        text.getElement().setProperty("innerHTML", getTranslation(key));
        return text;
    }

}
