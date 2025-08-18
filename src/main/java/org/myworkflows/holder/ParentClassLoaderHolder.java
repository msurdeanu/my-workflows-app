package org.myworkflows.holder;

import lombok.Getter;
import lombok.Setter;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Getter
public enum ParentClassLoaderHolder {
    INSTANCE;

    @Setter
    private ClassLoader classLoader = getClass().getClassLoader();

    public void resetClassLoaderToDefault() {
        classLoader = getClass().getClassLoader();
    }

}
