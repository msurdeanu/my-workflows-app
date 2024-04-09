package org.myworkflows.domain.command;

import com.sshtools.client.SshClient;
import com.sshtools.common.ssh.SshException;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;
import org.myworkflows.domain.command.api.OptionalParam;
import org.myworkflows.domain.command.output.SshCommandOutput;

import java.io.IOException;

import static java.util.Optional.ofNullable;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
public final class SshExecCommand extends AbstractCommand {

    @ExecutionMethod
    public SshCommandOutput sshExec(@MandatoryParam final String host,
                                    @MandatoryParam final String username,
                                    @MandatoryParam final String password,
                                    @MandatoryParam final String command,
                                    @OptionalParam final Integer port,
                                    @OptionalParam final Long timeout) throws IOException, SshException {
        final var resolvedPort = ofNullable(port).orElse(22);
        final var resolvedTimeout = ofNullable(timeout).orElse(60_000L);

        try (SshClient sshclient = new SshClient(host, resolvedPort, username, password.toCharArray())) {
            final var outputBuilder = new StringBuffer();
            final var status = sshclient.executeCommandWithResult(command, outputBuilder, resolvedTimeout);
            return SshCommandOutput.builder().exitCode(status).output(outputBuilder.toString()).build();
        }
    }

}
