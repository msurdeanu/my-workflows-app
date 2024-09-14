package org.myworkflows.domain.command;

import com.sshtools.client.SessionChannelNG;
import com.sshtools.client.SshClient;
import com.sshtools.client.shell.ExpectShell;
import com.sshtools.client.shell.ShellTimeoutException;
import com.sshtools.client.tasks.ShellTask;
import com.sshtools.common.ssh.SshException;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;
import org.myworkflows.domain.command.api.OptionalParam;
import org.myworkflows.domain.command.output.SshCommandOutput;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class SshShellCommand extends AbstractCommand {

    public SshShellCommand(String name,
                           Set<ExpressionNameValue> ifs,
                           Set<ExpressionNameValue> inputs,
                           Set<ExpressionNameValue> asserts,
                           Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod
    public SshCommandOutput sshShell(@MandatoryParam String host,
                                     @MandatoryParam String username,
                                     @MandatoryParam String password,
                                     @MandatoryParam List<String> commands,
                                     @OptionalParam Integer port,
                                     @OptionalParam Long timeout) throws IOException, SshException {
        final var resolvedPort = ofNullable(port).orElse(22);
        final var resolvedTimeout = ofNullable(timeout).orElse(60_000L);

        final var outputBuilder = new StringBuilder();
        final var commandOutputBuilder = SshCommandOutput.builder();

        try (SshClient sshclient = SshClient.SshClientBuilder.create()
            .withHostname(host)
            .withPort(resolvedPort)
            .withUsername(username)
            .withPassword(password.toCharArray())
            .build()) {
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
