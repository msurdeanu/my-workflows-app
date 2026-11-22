package org.myworkflows.domain.download;

import com.sshtools.client.SshClient;
import com.sshtools.client.sftp.SftpClient;
import com.sshtools.client.sftp.SftpFile;
import com.sshtools.common.sftp.SftpFileAttributes;
import com.sshtools.common.sftp.SftpStatusException;
import com.sshtools.common.ssh.SshException;
import lombok.RequiredArgsConstructor;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Reads a file or a directory of another server over SFTP, a file being streamed as it comes.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@RequiredArgsConstructor
final class SftpDownloadReader implements DownloadReader {

    private final SshClient sshClient;
    private final SftpClient sftpClient;
    private final String path;
    private final String name;
    private final SftpFileAttributes attributes;

    static SftpDownloadReader open(SshClient sshClient, SftpClient sftpClient, String path) throws IOException {
        try {
            return new SftpDownloadReader(sshClient, sftpClient, path, RemoteDownloadSource.nameOf(path), sftpClient.stat(path));
        } catch (Exception exception) {
            sftpClient.close();
            throw toIOException("'" + path + "' cannot be read over SFTP", exception);
        }
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public boolean isDirectory() {
        return attributes.isDirectory();
    }

    @Override
    public List<DownloadEntry> entries() throws IOException {
        if (!isDirectory()) {
            return List.of(new DownloadEntry(path, name, attributes.size().longValue()));
        }
        final var entries = new ArrayList<DownloadEntry>();
        try {
            list(path.replaceAll("/+$", ""), name, entries);
        } catch (Exception exception) {
            throw toIOException("Directory '" + path + "' cannot be listed over SFTP", exception);
        }
        entries.sort(Comparator.comparing(DownloadEntry::path));
        return entries;
    }

    @Override
    public void copy(DownloadEntry entry, OutputStream outputStream) throws IOException {
        try {
            sftpClient.get(entry.location(), outputStream);
        } catch (Exception exception) {
            throw toIOException("'" + entry.location() + "' cannot be copied over SFTP", exception);
        }
    }

    @Override
    public void close() throws IOException {
        try {
            sftpClient.close();
        } finally {
            sshClient.close();
        }
    }

    /**
     * The attributes of a listed file are the ones of the link when it is one, which is left out like on this server.
     */
    private void list(String directory, String zipPath, List<DownloadEntry> entries) throws SftpStatusException, SshException {
        for (SftpFile file : sftpClient.ls(directory)) {
            final var fileName = file.getFilename();
            if (".".equals(fileName) || "..".equals(fileName)) {
                continue;
            }
            final var fileAttributes = file.attributes();
            final var location = directory + "/" + fileName;
            if (fileAttributes.isDirectory()) {
                list(location, zipPath + "/" + fileName, entries);
            } else if (fileAttributes.isFile()) {
                entries.add(new DownloadEntry(location, zipPath + "/" + fileName, fileAttributes.size().longValue()));
            }
        }
    }

    private static IOException toIOException(String message, Exception exception) {
        if (exception instanceof SftpStatusException status && status.getStatus() == SftpStatusException.SSH_FX_NO_SUCH_FILE) {
            return new IOException(message + ": there is no such file or directory.", exception);
        }
        return exception instanceof IOException ioException ? ioException : new IOException(message + ": " + exception.getMessage(), exception);
    }

}
