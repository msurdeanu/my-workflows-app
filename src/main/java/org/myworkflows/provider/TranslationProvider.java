package org.myworkflows.provider;

import com.vaadin.flow.i18n.I18NProvider;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.ocpsoft.prettytime.PrettyTime;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import static java.lang.String.format;
import static java.util.Locale.ENGLISH;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class TranslationProvider implements I18NProvider {

    private static final PrettyTime PRETTY_TIME = new PrettyTime();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle("translation", ENGLISH);

    @Override
    public List<Locale> getProvidedLocales() {
        return List.of(ENGLISH);
    }

    @Override
    public String getTranslation(final String key, final Locale locale, final Object... params) {
        if (key == null) {
            return null;
        }

        if ("pretty.time.format".equals(key) && params.length == 1 && params[1] instanceof Instant instant) {
            return PRETTY_TIME.format(instant);
        }

        if (RESOURCE_BUNDLE.containsKey(key)) {
            return format(RESOURCE_BUNDLE.getString(key), params);
        }

        log.warn("Missing translation for key '{}'", key);
        return key;
    }

}
