package org.myworkflows.domain.ai;

import org.jspecify.annotations.Nullable;

/**
 * What became of one call of a batch: its answer, or why it has none.
 *
 * @param answer the answer, when Claude gave one.
 * @param failure why there is no answer - an error, a refusal, a batch which was cancelled or expired -, as the run of
 *     a command which called Claude would fail with it.
 * @author Mihai Surdeanu
 * @since 2.5
 */
public record AiBatchResult(@Nullable AiAnswer answer, @Nullable String failure) {

    public static AiBatchResult answered(AiAnswer answer) {
        return new AiBatchResult(answer, null);
    }

    public static AiBatchResult failed(String failure) {
        return new AiBatchResult(null, failure);
    }

    public boolean isAnswered() {
        return answer != null;
    }

}
