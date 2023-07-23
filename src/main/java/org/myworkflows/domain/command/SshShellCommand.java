package org.myworkflows.domain.command;

import com.sshtools.client.SessionChannelNG;
import com.sshtools.client.SshClient;
import com.sshtools.client.shell.ExpectShell;
import com.sshtools.client.shell.ShellTimeoutException;
import com.sshtools.client.tasks.ShellTask;
import com.sshtools.common.ssh.SshException;
import org.myworkflows.domain.command.output.SshCommandOutput;

import java.io.IOException;
import java.util.List;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SshShellCommand extends AbstractCommand {

    @ExecutionMethod
    public SshCommandOutput shell(@MandatoryParam final String host,
                                  @MandatoryParam final String username,
                                  @MandatoryParam final String password,
                                  @MandatoryParam final List<String> commands,
                                  @OptionalParam final Integer port,
                                  @OptionalParam final Long timeout) throws IOException, SshException {
        final var resolvedPort = ofNullable(port).orElse(22);
        final var resolvedTimeout = ofNullable(timeout).orElse(60_000L);

        final var outputBuilder = new StringBuilder();
        final var commandOutputBuilder = SshCommandOutput.builder();

        try (SshClient sshclient = new SshClient(host, resolvedPort, username, password.toCharArray())) {
            sshclient.runTask(new ShellTask(sshclient) {
                @Override
                protected void onOpenSession(SessionChannelNG sessionChannelNG) throws IOException, SshException, ShellTimeoutException {
                    final var shell = new ExpectShell(this);
                    for (String command : commands) {
                        final var shellProcess = shell.executeCommand(command);
                        shellProcess.drain();
                        commandOutputBuilder.exitCode(shellProcess.getExitCode());
                        outputBuilder.append(shellProcess.getCommandOutput());
                    }
                }
            }, resolvedTimeout);
        }

        return commandOutputBuilder.output(outputBuilder.toString()).build();
    }

}
