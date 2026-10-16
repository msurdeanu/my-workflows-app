package org.myworkflows.restapi.dto;

import java.util.List;

/**
 * Everything a template is made of: an update replaces all of it, so what is left out is gone afterwards.
 *
 * @param cron when the template runs by itself, as a Spring cron expression. Nothing, or a blank, for never.
 * @param enabled whether the cron starts the template. Nothing for no: a {@code boolean} left out would make the whole
 *     body unreadable, as a primitive cannot be null.
 * @param definitionIds the definitions the template runs, in the order it runs them.
 * @param parameterNames the parameters the template hands over to its run.
 * @author Mihai Surdeanu
 * @since 2.2
 */
public record WorkflowTemplateRequest(String name, String cron, Boolean enabled, List<Integer> definitionIds,
                                      List<String> parameterNames) {

    public boolean isEnabled() {
        return Boolean.TRUE.equals(enabled);
    }

}
