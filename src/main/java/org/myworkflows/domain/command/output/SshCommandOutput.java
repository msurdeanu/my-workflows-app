package org.myworkflows.domain.command.output;

import lombok.Builder;
import lombok.Getter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class SshCommandOutput {

    private final int exitCode;
    private final String output;

}
