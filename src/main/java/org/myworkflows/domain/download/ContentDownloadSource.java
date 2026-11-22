package org.myworkflows.domain.download;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * A text of the run, written as a UTF-8 file under the given name.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record ContentDownloadSource(String content, String name) implements DownloadSource {

    @Override
    public DownloadReader open() {
        final var bytes = content.getBytes(StandardCharsets.UTF_8);
        return new DownloadReader() {

            @Override
            public String name() {
                return name;
            }

            @Override
            public boolean isDirectory() {
                return false;
            }

            @Override
            public List<DownloadEntry> entries() {
                return List.of(new DownloadEntry(name, name, bytes.length));
            }

            @Override
            public void copy(DownloadEntry entry, OutputStream outputStream) throws IOException {
                outputStream.write(bytes);
            }
        };
    }

}
