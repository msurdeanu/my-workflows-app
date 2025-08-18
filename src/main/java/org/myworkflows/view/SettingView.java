package org.myworkflows.view;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationException;
import com.vaadin.flow.router.HasDynamicTitle;
import com.vaadin.flow.router.Route;
import jakarta.annotation.security.RolesAllowed;
import org.myworkflows.provider.SettingProvider;
import org.myworkflows.view.component.BaseLayout;
import org.myworkflows.view.component.ResponsiveLayout;
import org.myworkflows.view.transformer.SettingsToComponentsTransformer;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
@RolesAllowed("ROLE_ADMIN")
@Route(value = SettingView.ROUTE, layout = BaseLayout.class)
public class SettingView extends ResponsiveLayout implements HasDynamicTitle {

    public static final String ROUTE = "settings";

    private final Binder<SettingProvider> binder = new Binder<>();

    private final SettingProvider settingProvider;

    public SettingView(SettingProvider settingProvider) {
        this.settingProvider = settingProvider;

        add(createHeader(getTranslation("settings.page.subtitle")), createContent(createBody()), createFooter(settingProvider));
    }

    @Override
    public String getPageTitle() {
        return getTranslation("site.base.title", getTranslation("menu.main.settings"));
    }

    private Component createBody() {
        final var layout = new VerticalLayout();
        final var formLayout = new FormLayout();

        new SettingsToComponentsTransformer(binder).transform(settingProvider.getAll()).forEach(formLayout::add);
        binder.readBean(settingProvider);

        final var saveButton = createSaveButton();
        binder.addStatusChangeListener(event -> saveButton.setEnabled(event.getBinder().hasChanges() && event.getBinder().isValid()));
        formLayout.add(saveButton);

        formLayout.setResponsiveSteps(
            new FormLayout.ResponsiveStep("250px", 1),
            new FormLayout.ResponsiveStep("500px", 2),
            new FormLayout.ResponsiveStep("750px", 3),
            new FormLayout.ResponsiveStep("1000px", 4)
        );

        layout.add(formLayout);
        return layout;
    }

    private Button createSaveButton() {
        final var saveButton = new Button(getTranslation("settings.button.save"), event -> onClickListener());
        saveButton.setEnabled(false);

        return saveButton;
    }

    private void onClickListener() {
        try {
            binder.writeBean(settingProvider);
        } catch (ValidationException notUsed) {
            // Nothing to do
        }
    }

}

