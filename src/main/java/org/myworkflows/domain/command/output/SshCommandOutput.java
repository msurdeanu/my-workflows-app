package org.myworkflows.domain.command.output;

import lombok.Builder;
import lombok.Getter;

import java.io.Serializable;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Getter
@Builder
public class SshCommandOutput implements Serializable {

    private final int exitCode;
    private final String output;

}
