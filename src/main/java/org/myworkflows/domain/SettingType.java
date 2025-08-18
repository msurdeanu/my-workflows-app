package org.myworkflows.domain;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import static java.util.Arrays.stream;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
@RequiredArgsConstructor
public enum SettingType {

    STR("str"),
    STR_H("str_h"),
    TEXT("text"),
    TEXT_H("text_h"),
    PASSWORD("pass"),
    INTEGER("int"),
    INTEGER_H("int_h"),
    BOOLEAN("bool"),
    BOOLEAN_H("bool_h");

    @Getter
    private final String value;

    public static SettingType of(String value) {
        return stream(values())
            .filter(settingType -> settingType.getValue().equals(value))
            .findFirst()
            .orElse(null);
    }

}
