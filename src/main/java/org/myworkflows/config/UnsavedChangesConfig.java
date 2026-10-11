package org.myworkflows.config;

/**
 * Client side code executed on the workflow editor (lit-ace element) to tell whether its content differs from the saved
 * workflow definition. The content is compared in the browser and an unsaved-change event is dispatched only when the
 * answer flips, so typing costs no server round trip. MARK_SAVED_JS_CODE receives the saved content as $0 and may run
 * before the editor is ready.
 *
 * @author Mihai Surdeanu
 * @since 2.0
 */
public class UnsavedChangesConfig {

    public static final String JS_CODE = """
        const host = this;
        const aceEditor = host.editor;
        if (host.$unsavedChangesAttached) {
            return;
        }
        host.$unsavedChangesAttached = true;
        host.$unsaved = false;

        host.checkUnsavedChanges = () => {
            clearTimeout(host.$unsavedChangesTimer);
            const unsaved = host.$savedValue !== undefined && aceEditor.getValue().replace(/\\r\\n?/g, '\\n') !== host.$savedValue;
            if (unsaved !== host.$unsaved) {
                host.$unsaved = unsaved;
                host.dispatchEvent(new CustomEvent('unsaved-change', { detail: { unsaved: unsaved } }));
            }
        };
        // replacing the whole content is a removal followed by an insert: the check waits for the editor to settle
        aceEditor.on('change', () => {
            clearTimeout(host.$unsavedChangesTimer);
            host.$unsavedChangesTimer = setTimeout(host.checkUnsavedChanges, 100);
        });
        host.checkUnsavedChanges();
        """;

    public static final String MARK_SAVED_JS_CODE = """
        this.$savedValue = $0.replace(/\\r\\n?/g, '\\n');
        if (this.checkUnsavedChanges) {
            this.checkUnsavedChanges();
        }
        """;

}
