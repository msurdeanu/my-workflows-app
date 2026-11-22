package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.badge.BadgeVariant;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.confirmdialog.ConfirmDialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.ListItem;
import com.vaadin.flow.component.html.Paragraph;
import com.vaadin.flow.component.html.UnorderedList;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.RolesAllowed;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.Feature;
import org.myworkflows.domain.handler.FeatureEventHandler;
import org.myworkflows.exception.AiException;
import org.myworkflows.holder.AiClientHolder;
import org.myworkflows.service.FeatureService;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.FeatureGrid;
import org.myworkflows.view.component.PageGuide;
import org.myworkflows.view.component.ResponsiveLayout;

import java.util.Objects;
import java.util.stream.IntStream;

/**
 * The parts of the application which an admin turns on or off while it runs, one {@link Feature} per row. Turning
 * Claude on asks first, as from then on what its features send leaves the server.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@RolesAllowed("ROLE_ADMIN")
@Route(value = FeatureView.ROUTE, layout = BaseLayout.class)
public class FeatureView extends ResponsiveLayout implements HasDynamicTitle, FeatureEventHandler {

    public static final String ROUTE = "features";

    private static final int CLAUDE_SENDS_COUNT = 3;

    private static final int ERROR_NOTIFICATION_DURATION = 10_000;

    private final FeatureService featureService;

    private final FeatureGrid featureGrid;

    public FeatureView(FeatureService featureService) {
        super();
        this.featureService = featureService;
        featureGrid = new FeatureGrid(featureService, this);

        add(createHeader(getTranslation("features.page.title")));
        add(createContent(featureGrid));
        add(createGuide());
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.features"));
    }

    @Override
    public void onEnabledChanged(Feature feature, boolean enabled) {
        if (feature == Feature.CLAUDE && enabled) {
            askToTurnOnClaude();
        } else {
            change(feature, enabled);
        }
    }

    @Override
    public void onTestClaude() {
        try {
            final var check = AiClientHolder.INSTANCE.test();
            Notification.show(getTranslation("features.claude.test.success.message", check.model(), check.displayName(),
                    check.took().toMillis()))
                .addThemeVariants(NotificationVariant.SUCCESS);
        } catch (AiException exception) {
            Notification.show(getTranslation(exception.getReason().getTranslationKey(), Objects.toString(exception.getDetail(), StringUtils.EMPTY)),
                    ERROR_NOTIFICATION_DURATION, Notification.Position.BOTTOM_START)
                .addThemeVariants(NotificationVariant.ERROR);
        }
    }

    private void change(Feature feature, boolean enabled) {
        featureService.setEnabled(feature, enabled);
        featureGrid.refresh();
        Notification.show(getTranslation(feature.getTranslationPrefix() + (enabled ? ".turned-on" : ".turned-off")));
    }

    private void askToTurnOnClaude() {
        final var sends = new UnorderedList();
        IntStream.rangeClosed(1, CLAUDE_SENDS_COUNT)
            .forEach(number -> sends.add(new ListItem(getTranslation("dialog.feature-claude.sends." + number))));
        final var masked = new ListItem(getTranslation("dialog.feature-claude.masked"));
        masked.addClassName("feature-dialog-masked");
        sends.add(masked);
        final var warning = new Div(getTranslation("dialog.feature-claude.warning"));
        warning.addClassName("feature-dialog-warning");
        final var text = new Div(new Paragraph(getTranslation("dialog.feature-claude.text", AiClientHolder.INSTANCE.getDefaultModel())),
            sends, warning);
        text.addClassName("feature-dialog");

        final var dialog = new ConfirmDialog();
        // wide enough for the list to read well and for the buttons to keep their text on one line
        dialog.setWidth("480px");
        dialog.setHeader(getTranslation("dialog.feature-claude.header"));
        dialog.setText(text);
        dialog.setCancelable(true);
        dialog.setConfirmText(getTranslation("dialog.feature-claude.button.confirm"));
        dialog.addConfirmListener(_ -> change(Feature.CLAUDE, true));
        // the checkbox was ticked by the click which opened the dialog: unless Claude was turned on, closing the dialog
        // in any way takes it back, as the row is read again
        dialog.addOpenedChangeListener(event -> {
            if (!event.isOpened()) {
                featureGrid.refresh();
            }
        });
        dialog.open();
    }

    private PageGuide createGuide() {
        return createGuide("features")
            .legend("on", PageGuide.badge(getTranslation("features.grid.status.on"), BadgeVariant.SUCCESS))
            .legend("off", createNeutralBadge(getTranslation("features.grid.status.off")))
            .legend("no-key", PageGuide.badge(getTranslation("features.grid.status.no-key"), BadgeVariant.WARNING))
            .legend("enabled", new Checkbox(getTranslation("features.grid.enabled.column"), true))
            .legend("test", PageGuide.button(VaadinIcon.PLUG))
            .note("runtime")
            .note("first-start")
            .note("key", PageGuide.fields(AiClientHolder.API_KEY_VARIABLE, getTranslation("features.guide.notes.key.field")))
            .note("masked");
    }

    private static Component createNeutralBadge(String text) {
        final var badge = PageGuide.badge(text);
        badge.addClassName("transfer-neutral");
        return badge;
    }

}
