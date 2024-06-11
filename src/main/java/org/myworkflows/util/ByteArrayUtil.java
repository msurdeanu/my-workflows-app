package org.myworkflows.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ByteArrayUtil {

    public static Byte[] toObject(byte[] byteArray) {
        final var byteObjects = new Byte[byteArray.length];
        for (int i = 0; i < byteArray.length; i++) {
            byteObjects[i] = byteArray[i];
        }
        return byteObjects;
    }

    public static byte[] toPrimitive(Byte[] byteObjects) {
        final var byteArray = new byte[byteObjects.length];
        for (int i = 0; i < byteObjects.length; i++) {
            byteArray[i] = byteObjects[i];
        }
        return byteArray;
    }

}
