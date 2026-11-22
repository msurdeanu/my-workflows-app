package org.myworkflows.domain.download;

import lombok.Getter;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.OutputStream;

/**
 * Writes to a stream up to a number of bytes, and as long as the thread is not interrupted: a download of a run which is
 * cancelled, or which grows larger than allowed, stops at the next write. Whoever reads a source may wrap the exception
 * it throws, so it says itself why it stopped.
 *
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class LimitedOutputStream extends FilterOutputStream {

    private final long limit;

    @Getter
    private long written;

    @Getter
    private boolean limitExceeded;

    @Getter
    private boolean interrupted;

    public LimitedOutputStream(OutputStream outputStream, long limit) {
        super(outputStream);
        this.limit = limit;
    }

    @Override
    public void write(int oneByte) throws IOException {
        check(1);
        out.write(oneByte);
        written++;
    }

    @Override
    public void write(byte[] bytes, int offset, int length) throws IOException {
        check(length);
        out.write(bytes, offset, length);
        written += length;
    }

    private void check(int length) throws IOException {
        if (Thread.currentThread().isInterrupted()) {
            interrupted = true;
            throw new InterruptedIOException("The run was cancelled.");
        }
        if (written + length > limit) {
            limitExceeded = true;
            throw new IOException("The file is larger than the " + limit + " bytes allowed.");
        }
    }

}
