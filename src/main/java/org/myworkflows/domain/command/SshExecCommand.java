package org.myworkflows.domain.command;

import com.sshtools.client.SshClient;
import com.sshtools.common.ssh.SshException;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.domain.command.output.SshCommandOutput;

import java.io.IOException;
import java.util.Set;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@NoArgsConstructor
public final class SshExecCommand extends AbstractCommand {

    public static final String PREFIX = "sshExec";

    public SshExecCommand(String name,
                          Set<ExpressionNameValue> ifs,
                          Set<ExpressionNameValue> inputs,
                          Set<ExpressionNameValue> asserts,
                          Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = PREFIX)
    public SshCommandOutput sshExec(@ExecutionParam String host,
                                    @ExecutionParam String command,
                                    @ExecutionParam String username,
                                    @ExecutionParam String password,
                                    @ExecutionParam(required = false, defaultValue = "22") Number port,
                                    @ExecutionParam(required = false, defaultValue = "60000") Number timeout) throws IOException, SshException {
        try (SshClient sshclient = SshClient.SshClientBuilder.create()
            .withHostname(host)
            .withPort(port.intValue())
            .withUsername(username)
            .withPassword(password.toCharArray())
            .build()) {
            final var outputBuilder = new StringBuffer();
            final var status = sshclient.executeCommandWithResult(command, outputBuilder, timeout.longValue());
            return SshCommandOutput.builder().exitCode(status).output(outputBuilder.toString()).build();
        }
    }

}
