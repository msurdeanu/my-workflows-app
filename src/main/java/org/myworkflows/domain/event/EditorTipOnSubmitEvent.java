package org.myworkflows.domain.event;

import lombok.Builder;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Builder
public record EditorTipOnSubmitEvent(int tipId) implements Event {

}