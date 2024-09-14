package org.myworkflows.domain.command;

import com.sshtools.client.SshClient;
import com.sshtools.common.ssh.SshException;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;
import org.myworkflows.domain.command.api.OptionalParam;
import org.myworkflows.domain.command.output.SshCommandOutput;

import java.io.IOException;
import java.util.Set;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class SshExecCommand extends AbstractCommand {

    public SshExecCommand(String name,
                          Set<ExpressionNameValue> ifs,
                          Set<ExpressionNameValue> inputs,
                          Set<ExpressionNameValue> asserts,
                          Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod
    public SshCommandOutput sshExec(@MandatoryParam String host,
                                    @MandatoryParam String username,
                                    @MandatoryParam String password,
                                    @MandatoryParam String command,
                                    @OptionalParam Integer port,
                                    @OptionalParam Long timeout) throws IOException, SshException {
        final var resolvedPort = ofNullable(port).orElse(22);
        final var resolvedTimeout = ofNullable(timeout).orElse(60_000L);

        try (SshClient sshclient = SshClient.SshClientBuilder.create()
            .withHostname(host)
            .withPort(resolvedPort)
            .withUsername(username)
            .withPassword(password.toCharArray())
            .build()) {
            final var outputBuilder = new StringBuffer();
            final var status = sshclient.executeCommandWithResult(command, outputBuilder, resolvedTimeout);
            return SshCommandOutput.builder().exitCode(status).output(outputBuilder.toString()).build();
        }
    }

}
