package org.myworkflows.domain.download;

import com.sshtools.client.AbstractSessionChannel;
import com.sshtools.client.SessionChannelNG;
import com.sshtools.client.SshClient;
import com.sshtools.common.ssh.SshException;
import lombok.RequiredArgsConstructor;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;

/**
 * Reads a file or a directory of another server with commands over SSH, for a server which does not offer SFTP: a
 * directory is listed with {@code find}, a file is the output of {@code cat}, written to the stream as it comes, so
 * that a binary file is copied as it is.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@RequiredArgsConstructor
final class ExecDownloadReader implements DownloadReader {

    private static final long EXIT_CODE_WAIT_IN_MILLIS = 5_000;

    private static final long EXIT_CODE_POLL_IN_MILLIS = 20;

    private final SshClient sshClient;
    private final String path;
    private final String name;
    private final boolean directory;
    private final long size;
    private final long timeout;

    static ExecDownloadReader open(SshClient sshClient, String path, long timeout) throws IOException {
        final var quotedPath = quote(path);
        final var answer = run(sshClient, "if [ -d " + quotedPath + " ]; then echo d; elif [ -f " + quotedPath + " ]; then echo f; wc -c < "
            + quotedPath + "; else echo n; fi", timeout).strip().split("\\s+");
        final var name = RemoteDownloadSource.nameOf(path);
        return switch (answer[0]) {
            case "d" -> new ExecDownloadReader(sshClient, path, name, true, DownloadEntry.UNKNOWN_SIZE, timeout);
            case "f" -> new ExecDownloadReader(sshClient, path, name, false, parseSize(answer), timeout);
            default -> throw new IOException("'" + path + "' cannot be read over SSH: there is no such file or directory.");
        };
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public boolean isDirectory() {
        return directory;
    }

    @Override
    public List<DownloadEntry> entries() throws IOException {
        if (!directory) {
            return List.of(new DownloadEntry(path, name, size));
        }
        final var directoryPath = path.replaceAll("/+$", "");
        // a name may hold a line break, never a NUL
        final var found = run(sshClient, "cd -- " + quote(directoryPath) + " && find . -type f -print0", timeout);
        return Arrays.stream(found.split("\0"))
            .filter(file -> file.startsWith("./"))
            .map(file -> file.substring(2))
            .sorted()
            .map(file -> new DownloadEntry(directoryPath + "/" + file, name + "/" + file, DownloadEntry.UNKNOWN_SIZE))
            .toList();
    }

    @Override
    public void copy(DownloadEntry entry, OutputStream outputStream) throws IOException {
        final SessionChannelNG channel;
        try {
            channel = sshClient.openSessionChannel();
        } catch (SshException exception) {
            throw new IOException("'" + entry.location() + "' cannot be read over SSH: " + exception.getMessage(), exception);
        }
        try {
            if (!channel.executeCommand("cat -- " + quote(entry.location())).waitFor(timeout).isSuccess()) {
                throw new IOException("'" + entry.location() + "' cannot be read over SSH: the server refused to run cat.");
            }
            channel.getInputStream().transferTo(outputStream);
            final var exitCode = awaitExitCode(channel);
            if (exitCode != 0 && exitCode != AbstractSessionChannel.EXITCODE_NOT_RECEIVED) {
                throw new IOException("'" + entry.location() + "' cannot be read over SSH: cat ended with exit code " + exitCode + ".");
            }
        } finally {
            channel.close();
        }
    }

    @Override
    public void close() throws IOException {
        sshClient.close();
    }

    /**
     * @return a text which the shell of the server reads as the given one, whatever it holds.
     */
    static String quote(String text) {
        return "'" + text.replace("'", "'\\''") + "'";
    }

    private static String run(SshClient sshClient, String command, long timeout) throws IOException {
        final var output = new StringBuffer();
        final var exitCode = sshClient.executeCommandWithResult(command, output, timeout);
        if (exitCode != 0) {
            throw new IOException("A command over SSH ended with exit code " + exitCode + ": " + output.toString().strip());
        }
        return output.toString();
    }

    private static long parseSize(String[] answer) {
        try {
            return answer.length > 1 ? Long.parseLong(answer[1]) : DownloadEntry.UNKNOWN_SIZE;
        } catch (NumberFormatException notASize) {
            return DownloadEntry.UNKNOWN_SIZE;
        }
    }

    /**
     * The exit code follows the output, sometimes a moment after its end.
     */
    private static int awaitExitCode(SessionChannelNG channel) throws InterruptedIOException {
        final var deadline = System.currentTimeMillis() + EXIT_CODE_WAIT_IN_MILLIS;
        try {
            while (channel.getExitCode() == AbstractSessionChannel.EXITCODE_NOT_RECEIVED && System.currentTimeMillis() < deadline) {
                Thread.sleep(EXIT_CODE_POLL_IN_MILLIS);
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new InterruptedIOException("The run was cancelled.");
        }
        return channel.getExitCode();
    }

}
