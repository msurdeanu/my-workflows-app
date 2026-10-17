package org.myworkflows.domain.search;

import org.jspecify.annotations.Nullable;

/**
 * One item found by the search of the header.
 *
 * @param id the id of a definition or of a template, by which its page opens it; missing for the other kinds, which
 *     are opened by their name.
 * @param excerpt the line of the script, of the value or of the documentation page which holds the text; missing when
 *     the name holds it.
 * @author Mihai Surdeanu
 * @since 2.3
 */
public record GlobalSearchResult(Kind kind, @Nullable Integer id, String name, @Nullable String excerpt) {

    /**
     * In the order the results of the same rank are listed in.
     */
    public enum Kind {
        DEFINITION,
        TEMPLATE,
        DOC_PAGE,
        COMMAND,
        PARAMETER,
        PLACEHOLDER;

        /**
         * @return whether only an admin may find an item of this kind, as only an admin may open its page.
         */
        public boolean isForAdmins() {
            return this == PARAMETER || this == PLACEHOLDER;
        }
    }

}
