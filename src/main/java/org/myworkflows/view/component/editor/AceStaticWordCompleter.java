package org.myworkflows.view.component.editor;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.List;

@Getter
public class AceStaticWordCompleter implements IAceWordCompleter {

    private List<String> words;
    private String category;
    private boolean keepCompleters;

    public AceStaticWordCompleter(List<String> words) {
        this(words, AceEditor.DEFAULT_STATIC_CATEGORY, false);
    }

    public AceStaticWordCompleter(List<String> words, boolean keepCompleters) {
        this(words, AceEditor.DEFAULT_STATIC_CATEGORY, keepCompleters);
    }

    public AceStaticWordCompleter(List<String> words, String category) {
        this(words, AceEditor.DEFAULT_STATIC_CATEGORY, false);
    }

    public AceStaticWordCompleter(List<String> words, String category, boolean keepCompleters) {
        this.keepCompleters = keepCompleters;
        this.category = category;
        this.words = words;
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