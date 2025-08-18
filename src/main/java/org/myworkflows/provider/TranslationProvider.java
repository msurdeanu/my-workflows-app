package org.myworkflows.provider;

import com.vaadin.flow.i18n.I18NProvider;
import groovy.lang.Tuple2;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import static java.lang.String.format;
import static java.util.Locale.ENGLISH;

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
            final var prettiedTime = prettyTime(instant);
            return getTranslation(prettiedTime.getV1(), locale, prettiedTime.getV2());
        }

        if (RESOURCE_BUNDLE.containsKey(key)) {
            return format(RESOURCE_BUNDLE.getString(key), params);
        }

        log.warn("Missing translation for key '{}'", key);
        return key;
    }

    private Tuple2<String, Long> prettyTime(Instant instant) {
        final var duration = Duration.between(instant, Instant.now());
        final var seconds = duration.getSeconds();
        if (seconds < 60) {
            return Tuple2.tuple("pretty.time.seconds", seconds);
        } else if (seconds < 3600) {
            return Tuple2.tuple("pretty.time.minutes", seconds / 60);
        } else if (seconds < 86400) {
            return Tuple2.tuple("pretty.time.hours", seconds / 3600);
        } else {
            return Tuple2.tuple("pretty.time.days", seconds / 86400);
        }
    }

}
