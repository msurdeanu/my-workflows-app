package org.myworkflows.view.component.editor;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.Focusable;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.HasStyle;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.component.dependency.NpmPackage;
import com.vaadin.flow.shared.Registration;
import lombok.Getter;
import org.myworkflows.view.component.editor.events.AceBlurChanged;
import org.myworkflows.view.component.editor.events.AceChanged;
import org.myworkflows.view.component.editor.events.AceForceSyncDomEvent;
import org.myworkflows.view.component.editor.events.AceForceSyncEvent;
import org.myworkflows.view.component.editor.events.AceSelectionChanged;
import org.myworkflows.view.component.editor.events.AceValueChanged;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static java.util.Optional.ofNullable;

@Tag("lit-ace")
@NpmPackage(value = "@f0rce/lit-ace", version = "1.11.1")
@JsModule("./@f0rce/lit-ace/lit-ace.js")
public class AceEditor extends Component implements HasSize, HasStyle, Focusable<AceEditor> {

    public static final String DEFAULT_STATIC_CATEGORY = "keyword";
    public static final String DEFAULT_DYNAMIC_CATEGORY = "dynamic";

    @Getter
    private String value = "";

    @Getter
    private int fontSize = 14;
    private boolean softTabs = true;

    @Getter
    private int tabSize = 4;

    @Getter
    private boolean wrap = false;

    @Getter
    private boolean autoComplete = false;

    @Getter
    private boolean initialFocus = false;

    @Getter
    private boolean readOnly = false;

    private boolean printMargin = false;

    @Getter
    private boolean showInvisibles = false;

    @Getter
    private boolean showGutter = true;

    @Getter
    private boolean hightlightActiveLine = true;

    @Getter
    private boolean displayIndentGuides = false;
    private boolean highlightSelectedWord = false;
    @Getter
    private AceSelection selection = new AceSelection();
    @Getter
    private AceCursorPosition cursorPosition = new AceCursorPosition();
    /**     * -- GETTER --
     * Returns if useWorker is enabled/disabled for the editor.
     *
     * @return boolean
     */
    @Getter
    private boolean useWorker = false;
    @Getter
    private boolean liveAutocompletion = false;
    @Getter
    private boolean enableSnippets = false;

    private List<IAceWordCompleter> customWordCompleter = new ArrayList<>();

    // Some internal checking
    private boolean hasBeenDetached = false;

    public AceEditor() {
        super.addListener(AceBlurChanged.class, this::updateEditor);
        super.addListener(AceForceSyncDomEvent.class, this::onForceSyncDomEvent);
        this.getElement().setProperty("theme", "eclipse");
        this.getElement().setProperty("mode", "yaml");
        this.setWidth("100%");
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        // the same time)
        if (hasBeenDetached) {
            if (!value.isEmpty()) {
                setValue(this.value);
                if (!selection.compareTo(new AceSelection())) {
                    setSelection(this.selection);
                }
            }
            if (!this.customWordCompleter.isEmpty()) {
                for (IAceWordCompleter completer : this.customWordCompleter) {
                    if (completer instanceof AceStaticWordCompleter) {
                        this.getElement().callJsFunction("addStaticWordCompleter", completer.toJson());
                    } else if (completer instanceof AceDynamicWordCompleter) {
                        this.getElement().callJsFunction("addDynamicWordCompleter", completer.toJson());
                    }
                }
            }
            hasBeenDetached = false;
        }
    }

    @Override
    protected void onDetach(DetachEvent detachEvent) {
        this.hasBeenDetached = true;
    }

    private void updateEditor(AceBlurChanged event) {
        this.selection = event.getSelection();
        this.cursorPosition = event.getCursorPosition();
        if (!this.value.equals(event.getValue())) {
            this.fireEvent(new AceValueChanged(event.getSource(), true, event.getValue()));
        }
        this.value = event.getValue();
    }

    private void onForceSyncDomEvent(AceForceSyncDomEvent event) {
        this.selection = event.getSelection();
        this.cursorPosition = event.getCursorPosition();
        if (!this.value.equals(event.getValue())) {
            this.fireEvent(new AceValueChanged(event.getSource(), true, event.getValue()));
        }
        this.value = event.getValue();

        this.fireEvent(
            new AceForceSyncEvent(
                event.getSource(),
                event.isFromClient(),
                event.getValue(),
                event.getSelection(),
                event.getCursorPosition()));
    }

    /**
     * Clears the value of the editor.
     */
    public void clear() {
        this.getElement().callJsFunction("setValue", "");
        if (!value.isEmpty()) {
            fireEvent(new AceValueChanged(this, false, ""));
        }
        value = "";
    }

    /**
     * Sets the value of the editor.
     *
     * @param value {@link String}
     */
    public void setValue(String value) {
        this.getElement().callJsFunction("setValue", ofNullable(value).orElse(""));
        if (!this.value.equals(value)) {
            this.fireEvent(new AceValueChanged(this, false, value));
        }
        this.value = value;
    }

    public void setFontSize(int value) {
        this.getElement().setProperty("fontSize", value);
        this.fontSize = value;
    }

    public void setSofttabs(boolean value) {
        this.getElement().setProperty("softtabs", value);
        this.softTabs = value;
    }

    public boolean isSofttabs() {
        return this.softTabs;
    }

    public void setTabSize(int value) {
        this.getElement().setProperty("tabSize", String.valueOf(value));
        this.tabSize = value;
    }

    public void setWrap(boolean wrap) {
        this.getElement().setProperty("wrap", wrap);
        this.wrap = wrap;
    }

    public void setAutoComplete(boolean value) {
        this.getElement().setProperty("enableAutocompletion", value);
        this.autoComplete = value;
    }

    public void setInitialFocus(boolean value) {
        this.getElement().setProperty("initialFocus", value);
        this.initialFocus = value;
    }

    public void setReadOnly(boolean value) {
        this.getElement().setProperty("readonly", value);
        this.readOnly = value;
    }

    public void setShowPrintMargin(boolean value) {
        this.getElement().setProperty("showPrintMargin", value);
        this.printMargin = value;
    }

    public boolean isShowPrintMargin() {
        return this.printMargin;
    }

    public void setShowInvisibles(boolean value) {
        this.getElement().setProperty("showInvisibles", value);
        this.showInvisibles = value;
    }

    public void setShowGutter(boolean value) {
        this.getElement().setProperty("showGutter", value);
        this.showGutter = value;
    }

    public void setHighlightActiveLine(boolean value) {
        this.getElement().setProperty("highlightActiveLine", value);
        this.hightlightActiveLine = value;
    }

    public void setDisplayIndentGuides(boolean value) {
        this.getElement().setProperty("displayIndentGuides", value);
        this.displayIndentGuides = value;
    }

    public void setHighlightSelectedWord(boolean value) {
        this.getElement().setProperty("highlightSelectedWord", value);
        this.highlightSelectedWord = value;
    }

    public boolean isHightlightSelectedWord() {
        return this.highlightSelectedWord;
    }

    public void setSelection(int startRow, int startColumn, int endRow, int endColumn) {
        int startNewRow = Math.abs(startRow);
        int startNewColumn = Math.abs(startColumn);
        int endNewRow = Math.abs(endRow);
        int endNewColumn = Math.abs(endColumn);
        if (endNewColumn < startNewColumn) {
            int tmp = startNewColumn;
            startNewColumn = endNewColumn;
            endNewColumn = tmp;
        }
        if (endNewRow < startNewRow) {
            int tmp = startNewRow;
            startNewRow = endNewRow;
            endNewRow = tmp;
        }

        String json = AceJson.generateSelectionJson(startNewRow, startNewColumn, endNewRow, endNewColumn);
        this.getElement().callJsFunction("setSelection", json);
    }

    public void setSelection(int startRow, int startColumn, int endRow, int endColumn, boolean focus) {
        this.setSelection(startRow, startColumn, endRow, endColumn);
        if (focus) {
            this.focus();
        }
    }

    public void setSelection(int startIndex, int endIndex) {
        int startNewIndex = startIndex;
        int endNewIndex = endIndex;
        if (endNewIndex < startNewIndex) {
            int tmp = startNewIndex;
            startNewIndex = endNewIndex;
            endNewIndex = tmp;
        }
        this.getElement().callJsFunction("calculateSelectionByIndices", startNewIndex, endNewIndex);
    }

    public void setSelection(int startIndex, int endIndex, boolean focus) {
        this.setSelection(startIndex, endIndex);
        if (focus) {
            this.focus();
        }
    }

    public void setSelection(AceSelection selection) {
        String json = AceJson.generateSelectionJson(selection);
        this.getElement().callJsFunction("setSelection", json);
    }

    public void setSelection(AceSelection selection, boolean focus) {
        this.setSelection(selection);
        if (focus) {
            this.focus();
        }
    }

    public void setUseWorker(boolean value) {
        this.getElement().setProperty("useWorker", value);
        this.useWorker = value;
    }

    public void setCursorPosition(int row, int column) {
        String json = AceJson.generateCursorPositionJson(Math.abs(row), Math.abs(column));
        this.getElement().callJsFunction("setCursorPosition", json);
    }

    public void setCursorPosition(int row, int column, boolean focus) {
        this.setCursorPosition(row, column);
        if (focus) {
            this.focus();
        }
    }

    public void setCursorPosition(int index) {
        this.getElement().callJsFunction("calculateCursorPositionFromIndex", index);
    }

    public void setCursorPosition(int index, boolean focus) {
        this.setCursorPosition(index);
        if (focus) {
            this.focus();
        }
    }

    public void setCursorPosition(AceCursorPosition cursorPosition) {
        String json = AceJson.generateCursorPositionJson(cursorPosition);
        this.getElement().callJsFunction("setCursorPosition", json);
    }

    public void setCursorPosition(AceCursorPosition cursorPosition, boolean focus) {
        this.setCursorPosition(cursorPosition);
        if (focus) {
            this.focus();
        }
    }

    public void setLiveAutocompletion(boolean value) {
        this.getElement().setProperty("enableLiveAutocompletion", value);
        this.liveAutocompletion = value;
    }

    public void setEnableSnippets(boolean value) {
        this.getElement().setProperty("enableSnippets", value);
        this.enableSnippets = value;
    }

    public void addStaticWordCompleter(List<String> words) {
        if (words.isEmpty()) {
            return;
        }

        AceStaticWordCompleter aswc = new AceStaticWordCompleter(words);

        this.getElement().callJsFunction("addStaticWordCompleter", aswc.toJson());
        this.customWordCompleter = new ArrayList<>(List.of(aswc));
    }

    public void addStaticWordCompleter(List<String> words, boolean keepCompleters) {
        if (words.isEmpty()) {
            return;
        }

        AceStaticWordCompleter aswc = new AceStaticWordCompleter(words, keepCompleters);

        this.getElement().callJsFunction("addStaticWordCompleter", aswc.toJson());

        if (keepCompleters) {
            this.customWordCompleter.add(aswc);
        } else {
            this.customWordCompleter = new ArrayList<>(List.of(aswc));
        }
    }

    public void addStaticWordCompleter(List<String> words, String category) {
        if (words.isEmpty()) {
            return;
        }

        AceStaticWordCompleter aswc = new AceStaticWordCompleter(words, category);

        this.getElement().callJsFunction("addStaticWordCompleter", aswc.toJson());
        this.customWordCompleter = new ArrayList<>(List.of(aswc));
    }

    public void addStaticWordCompleter(List<String> words, String category, boolean keepCompleters) {
        if (words.isEmpty()) {
            return;
        }

        AceStaticWordCompleter aswc = new AceStaticWordCompleter(words, category, keepCompleters);

        this.getElement().callJsFunction("addStaticWordCompleter", aswc.toJson());

        if (keepCompleters) {
            this.customWordCompleter.add(aswc);
        } else {
            this.customWordCompleter = new ArrayList<>(List.of(aswc));
        }
    }

    public void addStaticWordCompleter(AceStaticWordCompleter staticWordCompleter) {
        if (staticWordCompleter.getWords().isEmpty()) {
            return;
        }

        this.getElement().callJsFunction("addStaticWordCompleter", staticWordCompleter.toJson());

        if (staticWordCompleter.isKeepCompleters()) {
            this.customWordCompleter.add(staticWordCompleter);
        } else {
            this.customWordCompleter = new ArrayList<>(List.of(staticWordCompleter));
        }
    }

    public List<AceStaticWordCompleter> getStaticWordCompleter() {
        ArrayList<AceStaticWordCompleter> staticCompleter = new ArrayList<>();

        for (IAceWordCompleter completer : this.customWordCompleter) {
            if (completer instanceof AceStaticWordCompleter) {
                staticCompleter.add((AceStaticWordCompleter) completer);
            }
        }

        return staticCompleter;
    }

    public void disableCustomAutocompletion() {
        this.disableCustomAutocompletion(true);
    }

    public void disableCustomAutocompletion(boolean useDefault) {
        this.getElement().callJsFunction("disableCustomAutocompletion", useDefault);
        this.customWordCompleter = new ArrayList<>();
    }

    public void addTextAtPosition(int row, int column, String text) {
        this.getElement().callJsFunction("insertText", row, column, text);
    }

    public void addTextAtCurrentPosition(String text) {
        this.addTextAtPosition(this.cursorPosition.getRow(), this.cursorPosition.getColumn(), text);
    }

    public Registration addSelectionChangeListener(
        ComponentEventListener<AceSelectionChanged> listener) {
        return this.addListener(AceSelectionChanged.class, listener);
    }

    public void sync() {
        this.getElement().callJsFunction("forceSync");
    }

    public Registration addSyncCompletedListener(ComponentEventListener<AceForceSyncEvent> listener) {
        return this.addListener(AceForceSyncEvent.class, listener);
    }

    public Registration addAceChangedListener(ComponentEventListener<AceChanged> listener) {
        return this.addListener(AceChanged.class, listener);
    }

    @Override
    public void focus() {
        this.getElement().callJsFunction("focusEditor");
    }

    public void runAfterSync(Runnable action) {
        Objects.requireNonNull(action);
        this.addListener(AceForceSyncDomEvent.class, event -> this.runAfterSync(event, action));
        this.sync();
    }

    private void runAfterSync(AceForceSyncDomEvent event, Runnable action) {
        event.unregisterListener();
        action.run();
    }

    public void addDynamicWordCompleter(Map<String, List<String>> dynamicWords, String seperator) {
        if (dynamicWords.isEmpty()) {
            return;
        }

        AceDynamicWordCompleter adwc = new AceDynamicWordCompleter(dynamicWords, seperator);

        this.getElement().callJsFunction("addDynamicWordCompleter", adwc.toJson());
        this.customWordCompleter = new ArrayList<>(Arrays.asList(adwc));
    }

    public void addDynamicWordCompleter(
        Map<String, List<String>> dynamicWords, String seperator, String category) {
        if (dynamicWords.isEmpty()) {
            return;
        }

        AceDynamicWordCompleter adwc = new AceDynamicWordCompleter(dynamicWords, seperator, category);

        this.getElement().callJsFunction("addDynamicWordCompleter", adwc.toJson());
        this.customWordCompleter = new ArrayList<>(Arrays.asList(adwc));
    }

    public void addDynamicWordCompleter(
        Map<String, List<String>> dynamicWords, String seperator, boolean keepCompleters) {
        if (dynamicWords.isEmpty()) {
            return;
        }

        AceDynamicWordCompleter adwc =
            new AceDynamicWordCompleter(dynamicWords, seperator, keepCompleters);

        this.getElement().callJsFunction("addDynamicWordCompleter", adwc.toJson());

        if (keepCompleters) {
            this.customWordCompleter.add(adwc);
        } else {
            this.customWordCompleter = new ArrayList<>(Arrays.asList(adwc));
        }
    }

    public void addDynamicWordCompleter(
        Map<String, List<String>> dynamicWords,
        String seperator,
        String category,
        boolean keepCompleters) {
        if (dynamicWords.isEmpty()) {
            return;
        }

        AceDynamicWordCompleter adwc =
            new AceDynamicWordCompleter(dynamicWords, seperator, category, keepCompleters);

        this.getElement().callJsFunction("addDynamicWordCompleter", adwc.toJson());

        if (keepCompleters) {
            this.customWordCompleter.add(adwc);
        } else {
            this.customWordCompleter = new ArrayList<>(Arrays.asList(adwc));
        }
    }

    public void addDynamicWordCompleter(AceDynamicWordCompleter dynamicWordCompleter) {
        if (dynamicWordCompleter.getDynamicWords().isEmpty()) {
            return;
        }

        this.getElement().callJsFunction("addStaticWordCompleter", dynamicWordCompleter.toJson());

        if (dynamicWordCompleter.isKeepCompleters()) {
            this.customWordCompleter.add(dynamicWordCompleter);
        } else {
            this.customWordCompleter = new ArrayList<>(Arrays.asList(dynamicWordCompleter));
        }
    }

    public List<AceDynamicWordCompleter> getDynamicWordCompleter() {
        ArrayList<AceDynamicWordCompleter> dynamicCompleter = new ArrayList<>();

        for (IAceWordCompleter completer : this.customWordCompleter) {
            if (completer instanceof AceDynamicWordCompleter) {
                dynamicCompleter.add((AceDynamicWordCompleter) completer);
            }
        }

        return dynamicCompleter;
    }

    public List<IAceWordCompleter> getWordCompleter() {
        return this.customWordCompleter;
    }

    public void openAutocompletion() {
        this.getElement().callJsFunction("openAutocompletion");
    }

    public void findAndSelect(String text) {
        this.getElement().callJsFunction("findAndSelect", text);
    }

    public Registration addValueChangeListener(ComponentEventListener<AceValueChanged> listener) {
        return this.addListener(AceValueChanged.class, listener);
    }

}