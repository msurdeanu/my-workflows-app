package org.myworkflows.holder;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class ParentClassLoaderHolderTest {

    @Test
    public void testParentClassLoaderHolder() {
        // when and then
        assertEquals(getClass().getClassLoader(), ParentClassLoaderHolder.INSTANCE.getClassLoader());
        ParentClassLoaderHolder.INSTANCE.setClassLoader(null);
        assertNull(ParentClassLoaderHolder.INSTANCE.getClassLoader());
        ParentClassLoaderHolder.INSTANCE.resetClassLoaderToDefault();
        assertEquals(getClass().getClassLoader(), ParentClassLoaderHolder.INSTANCE.getClassLoader());
    }

}
