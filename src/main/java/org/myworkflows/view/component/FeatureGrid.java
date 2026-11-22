package org.myworkflows.view.component;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.badge.Badge;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.Feature;
import org.myworkflows.domain.handler.FeatureEventHandler;
import org.myworkflows.holder.AiClientHolder;
import org.myworkflows.service.FeatureService;
import org.myworkflows.view.component.html.StandardPaginatedGrid;

import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

/**
 * One row per {@link Feature}: what turning it on means, what it needs to work, and a checkbox which turns it on or
 * off at once.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@RequiredArgsConstructor
public final class FeatureGrid extends Composite<VerticalLayout> {

    private final StandardPaginatedGrid<Feature, ?> paginatedGrid = new StandardPaginatedGrid<>();

    private final FeatureService featureService;

    private final FeatureEventHandler featureEventHandler;

    /**
     * Reads every row again: after a change, and after one which was refused or cancelled, to take it back.
     */
    public void refresh() {
        paginatedGrid.refreshCurrentPage();
    }

    /**
     * @return how many short items say what turning the given feature on means: the translations
     *     {@code <prefix>.means.1}, {@code <prefix>.means.2}...
     */
    public static int countMeans(Feature feature) {
        return switch (feature) {
            case CLAUDE -> 3;
            case REST_API -> 2;
            case EDITOR_TIPS -> 1;
        };
    }

    /**
     * @return the icon a feature is drawn with, on this page and in the search.
     */
    public static VaadinIcon iconOf(Feature feature) {
        return switch (feature) {
            case REST_API -> VaadinIcon.CODE;
            case CLAUDE -> VaadinIcon.MAGIC;
            case EDITOR_TIPS -> VaadinIcon.LIGHTBULB;
        };
    }

    @Override
    protected VerticalLayout initContent() {
        final var layout = super.initContent();
        layout.setSizeFull();

        final var narrowColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderPhoneCell))
            .setHeader(getTranslation("features.grid.feature.column"));
        final var featureColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderFeature))
            .setHeader(getTranslation("features.grid.feature.column"))
            .setWidth("260px")
            .setFlexGrow(2);
        final var meansColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderMeans))
            .setHeader(getTranslation("features.grid.means.column"))
            .setWidth("260px")
            .setFlexGrow(3);
        final var needsColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderNeeds))
            .setHeader(getTranslation("features.grid.needs.column"))
            .setWidth("200px")
            .setFlexGrow(1);
        final var statusColumn = paginatedGrid.addColumn(new ComponentRenderer<>(this::renderStatus))
            .setHeader(getTranslation("features.grid.status.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        final var enabledColumn = paginatedGrid.addColumn(new ComponentRenderer<>(feature -> renderEnabled(feature, false)))
            .setHeader(getTranslation("features.grid.enabled.column"))
            .setAutoWidth(true)
            .setFlexGrow(0);
        paginatedGrid.setNarrowColumn(narrowColumn, List.of(featureColumn, meansColumn, needsColumn, statusColumn, enabledColumn));
        paginatedGrid.setDataProvider(DataProvider.ofCollection(Arrays.asList(Feature.values())));

        layout.add(paginatedGrid);
        return layout;
    }

    /**
     * A phone shows the whole feature in one cell: what it is, whether it is on, then what it means and needs.
     */
    private Component renderPhoneCell(Feature feature) {
        final var state = new HorizontalLayout(renderStatus(feature), renderEnabled(feature, true));
        state.addClassName("cell-badges");
        state.setAlignItems(FlexComponent.Alignment.CENTER);
        final var layout = new VerticalLayout(renderFeature(feature), state, renderMeans(feature), renderNeeds(feature));
        layout.addClassName("phone-cell");
        layout.setPadding(false);
        layout.setSpacing(false);
        return layout;
    }

    private Component renderFeature(Feature feature) {
        final var icon = new Span(iconOf(feature).create());
        icon.addClassNames("feature-icon", "feature-icon-" + feature.name().toLowerCase().replace('_', '-'));
        icon.getElement().setAttribute("aria-hidden", "true");
        final var name = new Span(getTranslation(feature.getTranslationPrefix() + ".name"));
        name.addClassName("feature-name");
        final var description = new Span(getTranslation(feature.getTranslationPrefix() + ".description"));
        description.addClassName("cell-secondary");
        final var texts = new Div(name, description);
        texts.addClassName("feature-texts");
        final var layout = new Div(icon, texts);
        layout.addClassName("feature-cell");
        return layout;
    }

    private Component renderMeans(Feature feature) {
        final var layout = new HorizontalLayout();
        layout.addClassName("cell-badges");
        IntStream.rangeClosed(1, countMeans(feature)).forEach(number -> {
            final var badge = new Badge(getTranslation(feature.getTranslationPrefix() + ".means." + number));
            badge.addThemeVariants(BadgeVariant.SMALL);
            layout.add(badge);
        });
        if (feature == Feature.CLAUDE) {
            final var masked = new Badge(getTranslation("features.claude.masked"));
            masked.addThemeVariants(BadgeVariant.SMALL, BadgeVariant.WARNING);
            layout.add(masked);
        }
        return layout;
    }

    private Component renderNeeds(Feature feature) {
        if (feature != Feature.CLAUDE) {
            final var nothing = new Span(getTranslation("features.grid.needs.nothing"));
            nothing.addClassName("cell-secondary");
            return nothing;
        }

        final var hasKey = AiClientHolder.INSTANCE.hasKey();
        final var keyIcon = (hasKey ? VaadinIcon.CHECK : VaadinIcon.WARNING).create();
        keyIcon.addClassName(hasKey ? "feature-need-met" : "feature-need-missing");
        final var key = new Span(keyIcon, new Span(getTranslation(hasKey ? "features.claude.key.set" : "features.claude.key.missing",
            AiClientHolder.API_KEY_VARIABLE)));
        key.addClassName("feature-need");

        final var model = new Badge(AiClientHolder.INSTANCE.getDefaultModel());
        model.addThemeVariants(BadgeVariant.SMALL);
        model.addClassName("transfer-neutral");
        model.getElement().setAttribute("title", getTranslation("features.claude.model.tooltip"));

        final var testButton = new Button(getTranslation("features.claude.test.button"), VaadinIcon.PLUG.create(),
            _ -> featureEventHandler.onTestClaude());
        testButton.addThemeVariants(ButtonVariant.SMALL);
        testButton.setEnabled(hasKey);
        testButton.setTooltipText(getTranslation(hasKey ? "features.claude.test.tooltip" : "features.claude.test.no-key.tooltip"));

        final var layout = new VerticalLayout(key, model, testButton);
        layout.addClassName("feature-needs");
        layout.setPadding(false);
        layout.setSpacing(false);
        return layout;
    }

    private Component renderStatus(Feature feature) {
        final Badge badge;
        if (!featureService.isEnabled(feature)) {
            badge = new Badge(getTranslation("features.grid.status.off"));
            badge.addClassName("transfer-neutral");
        } else if (feature == Feature.CLAUDE && !AiClientHolder.INSTANCE.hasKey()) {
            badge = new Badge(getTranslation("features.grid.status.no-key"));
            badge.addThemeVariants(BadgeVariant.WARNING);
        } else {
            badge = new Badge(getTranslation("features.grid.status.on"));
            badge.addThemeVariants(BadgeVariant.SUCCESS);
        }
        badge.addThemeVariants(BadgeVariant.SMALL);
        return badge;
    }

    /**
     * @param labelled whether the checkbox names itself, as it has to where no column header does it.
     */
    private Component renderEnabled(Feature feature, boolean labelled) {
        final var checkbox = new Checkbox(featureService.isEnabled(feature));
        final var name = getTranslation(feature.getTranslationPrefix() + ".name");
        if (labelled) {
            checkbox.setLabel(getTranslation("features.grid.enabled.column"));
        }
        checkbox.setAriaLabel(getTranslation("features.grid.enabled.aria", name));
        checkbox.addValueChangeListener(event -> {
            if (event.isFromClient()) {
                featureEventHandler.onEnabledChanged(feature, event.getValue());
            }
        });
        return checkbox;
    }

}
