package org.myworkflows.domain.handler;

import java.io.InputStream;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public interface LibraryEventHandler {

    void onUpload(String fileName, InputStream inputStream);

}
