package org.myworkflows.view.component;

/**
 * @author Mihai Surdeanu
 * @since 2.0
 */
public enum Breakpoint {

    PHONE,
    TABLET,
    DESKTOP;

    private static final int PHONE_MAX_WIDTH = 640;
    private static final int TABLET_MAX_WIDTH = 1024;

    /**
     * Mirrors the breakpoints used by `styles.css`, so a layout decision taken here and one taken by a media
     * query always agree on what a phone is.
     */
    public static Breakpoint of(int width) {
        if (width <= PHONE_MAX_WIDTH) {
            return PHONE;
        }
        return width <= TABLET_MAX_WIDTH ? TABLET : DESKTOP;
    }

    public boolean isPhone() {
        return this == PHONE;
    }

}
