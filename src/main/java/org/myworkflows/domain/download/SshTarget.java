package org.myworkflows.domain.download;

/**
 * A server reached over SSH, with a password, the way the SSH commands reach it.
 *
 * @param timeout how long to wait for the server to answer, in milliseconds: to connect, and to a command which does
 *     not copy a file. A file takes the time it takes.
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record SshTarget(String host, int port, String username, String password, long timeout) {

    /**
     * @return where the server is, never the password, which an error message may show.
     */
    @Override
    public String toString() {
        return username + "@" + host + ":" + port;
    }

}
