package org.myworkflows.holder;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Setter
@Getter
@NoArgsConstructor
public final class ParentClassLoaderHolder {

    private static ParentClassLoaderHolder instance = null;

    private ClassLoader classLoader = getClass().getClassLoader();

    public static ParentClassLoaderHolder getInstance() {
        if (instance == null) {
            instance = new ParentClassLoaderHolder();
        }

        return instance;
    }

    public void resetClassLoaderToDefault() {
        classLoader = getClass().getClassLoader();
    }

}
