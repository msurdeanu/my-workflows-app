package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.WorkflowDownloadCompression;
import org.myworkflows.domain.WorkflowDownloadRequest;
import org.myworkflows.domain.WorkflowRun;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.domain.download.ContentDownloadSource;
import org.myworkflows.domain.download.DownloadSource;
import org.myworkflows.domain.download.LocalDownloadSource;
import org.myworkflows.domain.download.RemoteDownloadSource;
import org.myworkflows.domain.download.SshTarget;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.nio.file.Path;
import java.util.Set;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * Offers a file to download, which the run details and the development page show with a button: a text of the run, or
 * a file or a directory of this server or of another one, read over SSH. It is copied when the command runs, zipped when
 * it is a directory or large enough, and kept with its run.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
@NoArgsConstructor
public final class DownloadCommand extends AbstractCommand {

    public static final String PREFIX = "download";

    private static final long BYTES_IN_MB = 1024L * 1024L;

    public DownloadCommand(String name,
                           Set<ExpressionNameValue> ifs,
                           Set<ExpressionNameValue> inputs,
                           Set<ExpressionNameValue> asserts,
                           Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    /**
     * @param path a file or a directory, of this server unless a host is given.
     * @param content a text to offer as a file, instead of a path.
     * @param fileName the name of the file to download; the name of the path by default, and required for a content.
     * @param compress {@code auto}, {@code always} or {@code never}.
     * @param compressAbove the size in MB above which {@code auto} zips a file.
     * @param host the server to read the path from over SSH, which this server connects to.
     * @param timeout how long to wait for that server to answer, in milliseconds.
     * @return the id of the file, {@code <run id>/<file name>}.
     */
    @ExecutionMethod(prefix = PREFIX)
    public String download(@ExecutionParam(bypassed = true) WorkflowRun workflowRun,
                           @ExecutionParam(required = false) String path,
                           @ExecutionParam(required = false) String content,
                           @ExecutionParam(required = false) String fileName,
                           @ExecutionParam(required = false, defaultValue = "auto") String compress,
                           @ExecutionParam(required = false, defaultValue = "10") Number compressAbove,
                           @ExecutionParam(required = false) String host,
                           @ExecutionParam(required = false) String username,
                           @ExecutionParam(required = false) String password,
                           @ExecutionParam(required = false, defaultValue = "22") Number port,
                           @ExecutionParam(required = false, defaultValue = "60000") Number timeout) {
        final var compression = WorkflowDownloadCompression.of(compress);
        final var source = isNotBlank(host)
            ? toRemoteSource(new SshTarget(host.strip(), port.intValue(), username, password, timeout.longValue()), path, content)
            : toSource(path, content, fileName);
        final var request = new WorkflowDownloadRequest(source, fileName, compression, Math.round(compressAbove.doubleValue() * BYTES_IN_MB));
        return workflowRun.writeDownload(request).id();
    }

    /**
     * An input stays in the run once it is set, so a second download command sees the path or the content of the first
     * one: it has to empty the one it does not use.
     */
    private static DownloadSource toSource(String path, String content, String fileName) {
        if (isNotBlank(path) && content != null && !content.isEmpty()) {
            throw new WorkflowRuntimeException("A download is either 'download.path' or 'download.content', not both. A download command which "
                + "follows another one gets its inputs, so set the one it does not use to an empty text.");
        }
        if (isNotBlank(path)) {
            return new LocalDownloadSource(Path.of(path.strip()));
        }
        if (content == null || content.isEmpty()) {
            throw new WorkflowRuntimeException("A download needs 'download.path' or 'download.content'.");
        }
        if (isBlank(fileName)) {
            throw new WorkflowRuntimeException("A download of 'download.content' needs a 'download.fileName'.");
        }
        return new ContentDownloadSource(content, fileName.strip());
    }

    private static DownloadSource toRemoteSource(SshTarget target, String path, String content) {
        if (isBlank(path)) {
            throw new WorkflowRuntimeException("A download from '" + target.host() + "' needs the 'download.path' of a file or directory there.");
        }
        if (content != null && !content.isEmpty()) {
            throw new WorkflowRuntimeException("A download from '" + target.host() + "' reads 'download.path', so 'download.content' has to be "
                + "an empty text.");
        }
        if (isBlank(target.username()) || target.password() == null) {
            throw new WorkflowRuntimeException("A download from '" + target.host() + "' needs a 'download.username' and a 'download.password'.");
        }
        return new RemoteDownloadSource(target, path.strip());
    }

}
