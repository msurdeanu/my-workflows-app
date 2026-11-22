package org.myworkflows.domain.download;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class RemoteDownloadSourceTest {

    @Test
    public void whenPathEndsWithANameThenItIsTheNameOfTheDownload() {
        // when and then
        assertEquals("app", RemoteDownloadSource.nameOf("/var/log/app/"));
        assertEquals("today.log", RemoteDownloadSource.nameOf("/var/log/app/today.log"));
        assertEquals("today.log", RemoteDownloadSource.nameOf("today.log"));
        assertEquals("root", RemoteDownloadSource.nameOf("/"));
    }

    @Test
    public void whenPathIsQuotedThenTheShellReadsItAsItIs() {
        // when and then
        assertEquals("'/var/log/my app'", ExecDownloadReader.quote("/var/log/my app"));
        assertEquals("'/tmp/it'\\''s; rm -rf /'", ExecDownloadReader.quote("/tmp/it's; rm -rf /"));
    }

    @Test
    public void whenServerCannotBeReachedThenItSaysWhereWithoutThePassword() {
        // given
        final var source = new RemoteDownloadSource(new SshTarget("127.0.0.1", 1, "deploy", "secret", 2_000), "/var/log/app");

        // when and then
        final var exception = assertThrows(IOException.class, source::open);
        assertTrue(exception.getMessage().contains("deploy@127.0.0.1:1"));
        assertFalse(exception.getMessage().contains("secret"));
    }

    @Test
    public void whenLimitIsReachedThenTheStreamSaysSo() {
        // given
        final var outputStream = new LimitedOutputStream(new ByteArrayOutputStream(), 4);

        // when and then
        assertThrows(IOException.class, () -> outputStream.write(new byte[5], 0, 5));
        assertTrue(outputStream.isLimitExceeded());
        assertFalse(outputStream.isInterrupted());
        assertEquals(0, outputStream.getWritten());
    }

}
