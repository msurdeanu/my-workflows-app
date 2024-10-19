package org.myworkflows.domain.command;

import com.sshtools.client.SshClient;
import com.sshtools.client.shell.ExpectShell;
import com.sshtools.common.ssh.SshException;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.domain.command.output.SshCommandOutput;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static com.sshtools.client.tasks.ShellTask.ShellTaskBuilder.create;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class SshShellCommand extends AbstractCommand {

    public static final String PREFIX = "sshShell";

    public SshShellCommand(String name,
                           Set<ExpressionNameValue> ifs,
                           Set<ExpressionNameValue> inputs,
                           Set<ExpressionNameValue> asserts,
                           Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = PREFIX)
    public SshCommandOutput sshShell(@ExecutionParam String host,
                                     @ExecutionParam String username,
                                     @ExecutionParam String password,
                                     @ExecutionParam List<String> commands,
                                     @ExecutionParam(required = false, defaultValue = "22") Number port,
                                     @ExecutionParam(required = false, defaultValue = "60000") Number timeout) throws IOException, SshException {
        final var outputBuilder = new StringBuilder();
        final var commandOutputBuilder = SshCommandOutput.builder();

        try (SshClient sshclient = SshClient.SshClientBuilder.create()
            .withHostname(host)
            .withPort(port.intValue())
            .withUsername(username)
            .withPassword(password.toCharArray())
            .build()) {

            sshclient.runTask(create().withClient(sshclient).onTask((task, session) -> {
                final var shell = new ExpectShell(task);
                for (String command : commands) {
                    final var shellProcess = shell.executeCommand(command);
                    shellProcess.drain();
                    commandOutputBuilder.exitCode(shellProcess.getExitCode());
                    outputBuilder.append(shellProcess.getCommandOutput());
                }
            }).build(), timeout.longValue());
        }

        return commandOutputBuilder.output(outputBuilder.toString()).build();
    }

}
