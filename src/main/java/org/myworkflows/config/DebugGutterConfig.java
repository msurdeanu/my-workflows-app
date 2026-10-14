package org.myworkflows.config;

/**
 * Client side code executed on the workflow editor (lit-ace element) to debug a workflow from its gutter. A click next to
 * a line number dispatches a breakpoint-toggle event with the clicked row and the state of the editor; the server answers
 * with the rows to mark, as only the server knows which lines make which command. Whatever is marked is held by an Ace
 * anchor, so that it follows its line while the definition is edited:
 * <ul>
 *     <li>setBreakpointRows marks the rows given as $0, separated by commas, with a dot;</li>
 *     <li>setDebugCommand marks the command written from row $0 to row $1 as the one a run is paused before ($2 is
 *     'paused') or failed at ('failed'), and takes the mark away when $0 is negative;</li>
 *     <li>debugState returns, as JSON, the content of the editor, the row of its cursor and the rows of its breakpoints,
 *     all read at the same time.</li>
 * </ul>
 * The first two wait for the editor to take over a content set by the server, which it does later than the call is made.
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
public class DebugGutterConfig {

    public static final String JS_CODE = """
        const host = this;
        const aceEditor = host.editor;
        if (host.$debugGutterAttached) {
            return;
        }
        host.$debugGutterAttached = true;

        // the editor lives in the shadow root of lit-ace, which the styles of the page do not reach
        const gutterStyle = document.createElement('style');
        gutterStyle.textContent = `
            .ace_gutter .ace_gutter-cell { cursor: pointer; }
            .ace_gutter .ace_gutter-cell.mw-breakpoint::before { content: ''; position: absolute; left: 4px; top: 50%;
                width: 10px; height: 10px; margin-top: -5px; border-radius: 50%; background: var(--aura-red, #d8433a); }
            .ace_gutter .ace_gutter-cell.mw-paused { background-color: color-mix(in srgb, var(--aura-blue, #3153d4) 24%, transparent); }
            .ace_gutter .ace_gutter-cell.mw-paused::after { content: ''; position: absolute; left: 6px; top: 50%; margin-top: -5px;
                border-left: 8px solid var(--aura-blue, #3153d4); border-top: 5px solid transparent; border-bottom: 5px solid transparent; }
            .ace_gutter .ace_gutter-cell.mw-breakpoint.mw-paused::after { border-left-color: #fff; }
            .ace_gutter .ace_gutter-cell.mw-failed { background-color: color-mix(in srgb, var(--aura-red, #d8433a) 24%, transparent); }
            .ace_marker-layer .mw-paused-line { position: absolute; background: color-mix(in srgb, var(--aura-blue, #3153d4) 13%, transparent); }
            .ace_marker-layer .mw-failed-line { position: absolute; background: color-mix(in srgb, var(--aura-red, #d8433a) 13%, transparent); }`;
        host.shadowRoot.appendChild(gutterStyle);

        const Range = ace.require('ace/range').Range;
        let breakpoints = [];
        // the anchors of the first and of the last line of the command which is marked, and what it is marked as
        let command = [];
        let commandKind = '';
        let commandMarker = null;
        let drawing = null;

        const draw = () => {
            drawing = null;
            const session = aceEditor.session;
            session.clearBreakpoints();
            breakpoints.forEach(anchor => session.setBreakpoint(anchor.row, ' mw-breakpoint '));
            if (commandMarker !== null) {
                session.removeMarker(commandMarker);
                commandMarker = null;
            }
            if (command.length) {
                const firstRow = command[0].row;
                session.setBreakpoint(firstRow, (session.getBreakpoints()[firstRow] || '') + ' mw-' + commandKind + ' ');
                commandMarker = session.addMarker(new Range(firstRow, 0, Math.max(firstRow, command[1].row), 1),
                    'mw-' + commandKind + '-line', 'fullLine');
            }
        };
        // an edit can move several anchors: they are drawn once it is over
        const drawLater = () => {
            if (drawing === null) {
                drawing = setTimeout(draw, 0);
            }
        };
        const anchorAt = row => {
            const anchor = aceEditor.session.doc.createAnchor(Math.min(row, aceEditor.session.getLength() - 1), 0);
            anchor.on('change', drawLater);
            return anchor;
        };
        const release = anchors => anchors.forEach(anchor => anchor.detach());

        host.setBreakpointRows = rows => host.updateComplete.then(() => {
            release(breakpoints);
            breakpoints = rows.split(',').filter(row => row !== '').map(row => anchorAt(Number(row)));
            draw();
        });

        host.setDebugCommand = (firstRow, lastRow, kind) => host.updateComplete.then(() => {
            release(command);
            command = firstRow < 0 ? [] : [anchorAt(firstRow), anchorAt(lastRow)];
            commandKind = kind;
            draw();
            if (firstRow >= 0 && !aceEditor.isRowFullyVisible(firstRow)) {
                aceEditor.scrollToLine(firstRow, true, true, () => {});
            }
        });

        host.debugState = () => JSON.stringify({
            value: aceEditor.getValue(),
            cursor: aceEditor.getCursorPosition().row,
            breakpoints: breakpoints.map(anchor => anchor.row)
        });

        aceEditor.on('guttermousedown', event => {
            // folding a command stays what it is
            if (event.getButton() !== 0 || aceEditor.renderer.$gutterLayer.getRegion(event) === 'foldWidgets') {
                return;
            }
            host.dispatchEvent(new CustomEvent('breakpoint-toggle', {
                detail: { row: event.getDocumentPosition().row, state: host.debugState() }
            }));
            event.stop();
        });
        """;

}
