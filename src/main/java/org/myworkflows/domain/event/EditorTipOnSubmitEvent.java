package org.myworkflows.domain.event;

import lombok.Builder;
import lombok.Getter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Builder
@Getter
public class EditorTipOnSubmitEvent implements Event {

    private final int tipId;

}