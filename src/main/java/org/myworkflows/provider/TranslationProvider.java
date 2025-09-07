package org.myworkflows.provider;

import com.vaadin.flow.i18n.I18NProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import static java.lang.String.format;
import static java.util.Locale.ENGLISH;
import static org.myworkflows.util.LangUtil.pluralize;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Slf4j
@Component
@RequiredArgsConstructor
public final class TranslationProvider implements I18NProvider {

    public static final String PRETTY_TIME_FORMAT = "pretty.time.format";

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle("translation", ENGLISH);

    @Override
    public List<Locale> getProvidedLocales() {
        return List.of(ENGLISH);
    }

    @Override
    public String getTranslation(String key, Locale locale, Object... params) {
        if (key == null) {
            return null;
        }

        if (PRETTY_TIME_FORMAT.equals(key) && params.length == 1 && params[0] instanceof Instant instant) {
            return getTranslation("pretty.time.ago", locale, prettyTime(instant, locale));
        }

        if (RESOURCE_BUNDLE.containsKey(key)) {
            return format(RESOURCE_BUNDLE.getString(key), params);
        }

        log.warn("Missing translation for key '{}'", key);
        return key;
    }

    private String prettyTime(Instant instant, Locale locale) {
        final var duration = Duration.between(instant, Instant.now());
        final var seconds = duration.getSeconds();
        if (seconds < 60) {
            return getTranslation("pretty.time.second", locale);
        } else if (seconds < 3600) {
            return pluralize(getTranslation("pretty.time.minute", locale), seconds / 60).orElse(StringUtils.EMPTY);
        } else if (seconds < 86400) {
            return pluralize(getTranslation("pretty.time.hour", locale), seconds / 3600).orElse(StringUtils.EMPTY);
        } else {
            return pluralize(getTranslation("pretty.time.day", locale), seconds / 86400).orElse(StringUtils.EMPTY);
        }
    }

}
