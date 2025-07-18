package org.myworkflows.view.component.editor;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.List;
import java.util.Map;

@Getter
public class AceDynamicWordCompleter implements IAceWordCompleter {

    private Map<String, List<String>> dynamicWords;
    private String seperator;
    private String category;
    private boolean keepCompleters;

    public AceDynamicWordCompleter(Map<String, List<String>> dynamicWords, String seperator) {
        this(dynamicWords, seperator, AceEditor.DEFAULT_DYNAMIC_CATEGORY, false);
    }

    public AceDynamicWordCompleter(
        Map<String, List<String>> dynamicWords, String seperator, String category) {
        this(dynamicWords, seperator, category, false);
    }

    public AceDynamicWordCompleter(
        Map<String, List<String>> dynamicWords, String seperator, boolean keepCompleters) {
        this(dynamicWords, seperator, AceEditor.DEFAULT_DYNAMIC_CATEGORY, keepCompleters);
    }

    public AceDynamicWordCompleter(
        Map<String, List<String>> dynamicWords,
        String seperator,
        String category,
        boolean keepCompleters) {
        this.dynamicWords = dynamicWords;
        this.seperator = seperator;
        this.category = category;
        this.keepCompleters = keepCompleters;
    }

    @Override
    public String toJson() {
        try {
            return new ObjectMapper().writeValueAsString(this);
        } catch (JsonProcessingException e) {
            throw new WorkflowRuntimeException(e);
        }
    }

}
