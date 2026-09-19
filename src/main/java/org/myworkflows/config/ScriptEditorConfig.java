package org.myworkflows.config;

/**
 * Client side code executed on the workflow editor (lit-ace element) to edit java.script and groovy.script values in a
 * dedicated editor. A code lens above each script input and the Ctrl + Alt + J key binding dispatch a script-edit event
 * (with the requested row and the current definition), while replaceScriptBlock writes the edited script back as a
 * single undo step. The code lens titles are received as $0 (Java) and $1 (Groovy).
 *
 * @author Mihai Surdeanu
 * @since 1.3
 */
public class ScriptEditorConfig {

    public static final String JS_CODE = """
        const host = this;
        const aceEditor = host.editor;
        if (host.$scriptEditorAttached) {
            return;
        }
        host.$scriptEditorAttached = true;

        // Ace adds the code lens styles to the document head, but the editor lives in the shadow root of lit-ace
        const lensStyle = document.createElement('style');
        lensStyle.textContent = `
            .ace_codeLens { position: absolute; width: 100%; display: flex; align-items: flex-end; pointer-events: none;
                color: #8c8c8c; font-size: 88%; background: inherit; }
            .ace_codeLens > a { cursor: pointer; pointer-events: auto; }
            .ace_codeLens > a:hover { color: var(--aura-accent-color, #0000ff); text-decoration: underline; }`;
        host.shadowRoot.appendChild(lensStyle);

        aceEditor.commands.addCommand({
            name: 'openScriptEditor',
            bindKey: 'Ctrl-Alt-J',
            readOnly: true,
            exec: (editor, args) => host.dispatchEvent(new CustomEvent('script-edit', {
                detail: {
                    row: args && args.row !== undefined ? args.row : editor.getCursorPosition().row,
                    value: editor.getValue()
                }
            }))
        });

        const scriptInput = /^\\s*(?:-\\s+)?name\\s*:\\s*(['"]?)(java|groovy)\\.script\\1\\s*(?:#.*)?$/;
        ace.config.loadModule('ace/ext/code_lens', codeLens => codeLens.registerCodeLensProvider(aceEditor, {
            provideCodeLenses: (session, callback) => callback(null, session.getDocument().getAllLines()
                .map((line, row) => ({ row: row, match: scriptInput.exec(line) }))
                .filter(item => item.match)
                .map(item => ({
                    start: { row: item.row, column: 0 },
                    command: { id: 'openScriptEditor', title: item.match[2] === 'java' ? $0 : $1, arguments: { row: item.row } }
                })))
        }));

        const Range = ace.require('ace/range').Range;
        host.replaceScriptBlock = (startRow, startColumn, endRow, endColumn, source, text) => {
            const session = aceEditor.session;
            const range = new Range(startRow, startColumn, endRow, endColumn);
            if (session.getTextRange(range).replace(/\\r\\n?/g, '\\n') !== source) {
                return false;
            }
            session.markUndoGroup();
            session.replace(range, text);
            session.markUndoGroup();
            aceEditor.gotoLine(startRow + 1, startColumn);
            aceEditor.focus();
            host.forceSync();
            return true;
        };
        """;

}
