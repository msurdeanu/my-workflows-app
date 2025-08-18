package org.myworkflows.view.transformer;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.Setting;
import org.myworkflows.domain.SettingType;
import org.myworkflows.provider.SettingProvider;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
@Slf4j
@RequiredArgsConstructor
public class SettingsToComponentsTransformer implements Transformer<List<Setting>, List<Component>> {

    private final Map<SettingType, Function<Setting, Optional<Component>>> settingToComponentMapping = createMapper();

    private final Binder<SettingProvider> binder;

    @Override
    public List<Component> transform(List<Setting> settings) {
        return settings.stream().flatMap(setting -> createComponent(setting).stream()).collect(Collectors.toList());
    }

    private Optional<Component> createComponent(Setting setting) {
        return ofNullable(settingToComponentMapping.get(setting.getType()))
            .flatMap(function -> function.apply(setting));
    }

    private Map<SettingType, Function<Setting, Optional<Component>>> createMapper() {
        final var settingTypeObjectEnumMap = new EnumMap<SettingType, Function<Setting, Optional<Component>>>(SettingType.class);
        settingTypeObjectEnumMap.put(SettingType.STR, setting -> Optional.of(createTextField(setting)));
        settingTypeObjectEnumMap.put(SettingType.STR_H, setting -> Optional.empty());
        settingTypeObjectEnumMap.put(SettingType.TEXT, setting -> Optional.of(createTextArea(setting)));
        settingTypeObjectEnumMap.put(SettingType.TEXT_H, setting -> Optional.empty());
        settingTypeObjectEnumMap.put(SettingType.PASSWORD, setting -> Optional.of(createPasswordField(setting)));
        settingTypeObjectEnumMap.put(SettingType.INTEGER, setting -> Optional.of(createIntegerField(setting)));
        settingTypeObjectEnumMap.put(SettingType.INTEGER_H, setting -> Optional.empty());
        settingTypeObjectEnumMap.put(SettingType.BOOLEAN, setting -> Optional.of(createCheckbox(setting)));
        settingTypeObjectEnumMap.put(SettingType.BOOLEAN_H, setting -> Optional.empty());
        return settingTypeObjectEnumMap;
    }

    private Component createTextField(Setting setting) {
        final var textField = new TextField();
        textField.setLabel(textField.getTranslation(setting.getTitle()));
        textField.setHelperText(textField.getTranslation(setting.getDescription()));
        if (!setting.isEditable()) {
            textField.setReadOnly(true);
        }

        binder.forField(textField).bind(
            settingProvider -> settingProvider.getOrDefault(setting.getKey(), StringUtils.EMPTY),
            (settingProvider, newValue) -> settingProvider.set(setting.getKey(), newValue)
        );

        return textField;
    }

    private Component createTextArea(Setting setting) {
        final var textArea = new TextArea();
        textArea.setLabel(textArea.getTranslation(setting.getTitle()));
        textArea.setHelperText(textArea.getTranslation(setting.getDescription()));
        if (!setting.isEditable()) {
            textArea.setReadOnly(true);
        }

        binder.forField(textArea).bind(
            settingProvider -> settingProvider.getOrDefault(setting.getKey(), StringUtils.EMPTY),
            (settingProvider, newValue) -> settingProvider.set(setting.getKey(), newValue)
        );

        return textArea;
    }

    private PasswordField createPasswordField(Setting setting) {
        final var passwordField = new PasswordField();
        passwordField.setLabel(passwordField.getTranslation(setting.getTitle()));
        passwordField.setHelperText(passwordField.getTranslation(setting.getDescription()));
        if (!setting.isEditable()) {
            passwordField.setReadOnly(true);
        }

        binder.forField(passwordField).bind(
            settingProvider -> settingProvider.getOrDefault(setting.getKey(), StringUtils.EMPTY),
            (settingProvider, newValue) -> settingProvider.set(setting.getKey(), newValue)
        );

        return passwordField;
    }

    private IntegerField createIntegerField(Setting setting) {
        final var integerField = new IntegerField();
        integerField.setLabel(integerField.getTranslation(setting.getTitle()));
        integerField.setHelperText(integerField.getTranslation(setting.getDescription()));
        if (!setting.isEditable()) {
            integerField.setReadOnly(true);
        }

        binder.forField(integerField).bind(
            settingProvider -> settingProvider.getOrDefault(setting.getKey(), 0),
            (settingProvider, newValue) -> settingProvider.set(setting.getKey(), newValue)
        );

        return integerField;
    }

    private Checkbox createCheckbox(Setting setting) {
        final var checkbox = new Checkbox();
        checkbox.setLabel(checkbox.getTranslation(setting.getTitle()));
        checkbox.setHelperText(checkbox.getTranslation(setting.getDescription()));
        if (!setting.isEditable()) {
            checkbox.setReadOnly(true);
        }

        binder.forField(checkbox).bind(
            settingProvider -> settingProvider.getOrDefault(setting.getKey(), Boolean.FALSE),
            (settingProvider, newValue) -> settingProvider.set(setting.getKey(), newValue)
        );

        return checkbox;
    }

}
