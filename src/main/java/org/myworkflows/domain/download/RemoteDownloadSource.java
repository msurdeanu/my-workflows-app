package org.myworkflows.domain.download;

import com.sshtools.client.SshClient;
import com.sshtools.client.sftp.SftpClient;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;

/**
 * A file or a directory of another server, read over SFTP. A server which runs commands over SSH but does not offer
 * SFTP is read with {@code find} and {@code cat} instead, through the same connection.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@Slf4j
public record RemoteDownloadSource(SshTarget target, String path) implements DownloadSource {

    @Override
    public DownloadReader open() throws IOException {
        final var sshClient = connect();
        try {
            return openReader(sshClient);
        } catch (IOException | RuntimeException exception) {
            sshClient.close();
            throw exception;
        }
    }

    private DownloadReader openReader(SshClient sshClient) throws IOException {
        SftpClient sftpClient = null;
        try {
            sftpClient = SftpClient.SftpClientBuilder.create().withClient(sshClient).build();
        } catch (Exception sftpRefused) {
            log.info("SFTP is not offered by {}, so '{}' is read with commands over SSH instead.", target, path, sftpRefused);
        }
        return sftpClient == null
            ? ExecDownloadReader.open(sshClient, path, target.timeout())
            : SftpDownloadReader.open(sshClient, sftpClient, path);
    }

    private SshClient connect() throws IOException {
        try {
            return SshClient.SshClientBuilder.create()
                .withHostname(target.host())
                .withPort(target.port())
                .withUsername(target.username())
                .withPassword(target.password())
                .withConnectTimeout(target.timeout())
                .build();
        } catch (Exception exception) {
            throw new IOException("Could not connect over SSH to " + target + ": " + exception.getMessage(), exception);
        }
    }

    /**
     * @return the name of the file or directory at the end of a path of the server, which uses {@code /}.
     */
    static String nameOf(String path) {
        final var trimmed = path.replaceAll("/+$", "");
        final var name = trimmed.substring(trimmed.lastIndexOf('/') + 1);
        return name.isEmpty() ? "root" : name;
    }

}
