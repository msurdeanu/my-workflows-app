package org.myworkflows.util;

import java.util.regex.Matcher;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@FunctionalInterface
public interface StringReplacerCallback {

    String replace(final Matcher match);

}
