package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Hides secrets from whoever reads a value: the prints of a run, the variables of a paused one, and whatever is sent to
 * Claude. A value whose name - the name of a variable, or a key of a map - contains {@code password} is a secret,
 * however deep it sits. What goes to Claude is masked by value too: every occurrence of a value which is known to be a
 * secret, such as the one of a password parameter, wherever it stands in a text.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class SecretMasker {

    /**
     * A name which holds this, whatever its case, names a secret.
     */
    public static final String SECRET_NAME_PART = "password";

    /**
     * What stands for a secret found inside a text, whatever its length: the length of a password is a hint too.
     */
    public static final String MASK = "********";

    /**
     * A known secret shorter than this is not searched for in a text, where it would mask the parts of many words.
     */
    public static final int MIN_SECRET_LENGTH = 4;

    /**
     * Deeper than this, a value is masked as a whole: it is either very unusual or holds itself.
     */
    static final int MAX_DEPTH = 64;

    public static boolean isSecretName(String name) {
        return StringUtils.containsIgnoreCase(name, SECRET_NAME_PART);
    }

    /**
     * @return the given text with every character but the blanks replaced by {@code *}, the way a print shows a secret.
     */
    public static String maskCharacters(String value) {
        return value.replaceAll("\\S", "*");
    }

    /**
     * @return the given value, masked when its name names a secret.
     */
    public static String maskByName(String name, String value) {
        return isSecretName(name) ? maskCharacters(value) : value;
    }

    /**
     * @param secrets values which are known to be secrets; the blank ones and the ones shorter than
     *     {@link #MIN_SECRET_LENGTH} are left out.
     * @return the given text with every occurrence of a secret replaced by {@link #MASK}, the longest secrets first so
     *     that a secret which holds another is masked whole.
     */
    public static String maskText(String text, Collection<String> secrets) {
        if (StringUtils.isEmpty(text) || secrets.isEmpty()) {
            return text;
        }
        var masked = text;
        for (final var secret : searchable(secrets)) {
            masked = Strings.CS.replace(masked, secret, MASK);
        }
        return masked;
    }

    /**
     * Masks a value before it leaves the server. A map stays a map and a collection or an array becomes a list, with
     * what sits under a key naming a secret masked by {@link #maskCharacters(String)}; a text, and the text of any other
     * value, goes through {@link #maskText(String, Collection)}.
     *
     * @return a copy of the value made of maps, lists and texts, or {@code null} for {@code null}.
     */
    public static Object mask(Object value, Collection<String> secrets) {
        return mask(value, searchable(secrets), 0);
    }

    private static Object mask(Object value, List<String> secrets, int depth) {
        if (value == null) {
            return null;
        }
        if (depth > MAX_DEPTH) {
            return MASK;
        }
        return switch (value) {
            case Map<?, ?> map -> maskMap(map, secrets, depth);
            case Collection<?> collection -> maskList(collection, secrets, depth);
            case Object[] array -> maskList(List.of(array), secrets, depth);
            default -> maskText(String.valueOf(value), secrets);
        };
    }

    private static Map<String, Object> maskMap(Map<?, ?> map, List<String> secrets, int depth) {
        final var masked = new LinkedHashMap<String, Object>();
        map.forEach((key, item) -> {
            final var name = String.valueOf(key);
            masked.put(maskText(name, secrets), isSecretName(name)
                ? maskCharacters(String.valueOf(item))
                : mask(item, secrets, depth + 1));
        });
        return masked;
    }

    private static List<Object> maskList(Collection<?> collection, List<String> secrets, int depth) {
        final var masked = new ArrayList<>(collection.size());
        collection.forEach(item -> masked.add(mask(item, secrets, depth + 1)));
        return masked;
    }

    private static List<String> searchable(Collection<String> secrets) {
        return secrets.stream()
            .filter(Objects::nonNull)
            .filter(secret -> !StringUtils.isBlank(secret) && secret.length() >= MIN_SECRET_LENGTH)
            .distinct()
            .sorted(Comparator.comparingInt(String::length).reversed())
            .toList();
    }

}
