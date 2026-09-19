package org.myworkflows.view.util;

import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 1.3
 */
public final class ScriptBlockUtilTest {

    private static final String DEFINITION = """
        commands:
          - name: Run method which returns 0
            class: java
            inputs:
              - name: java.script
                value: |
                  import org.myworkflows.domain.WorkflowRunCache;
                  public class DynamicClass {
                    public int run(WorkflowRunCache cache) {
                      return 0;
                    }
                  }
              - name: java.method
                value: run
          - name: Run groovy script
            class: groovy
            inputs:
              - name: groovy.script
                value: "def run(cache) { 1 }" # inline script
        """;

    private static final String JAVA_SCRIPT = """
        import org.myworkflows.domain.WorkflowRunCache;
        public class DynamicClass {
          public int run(WorkflowRunCache cache) {
            return 0;
          }
        }""";

    @Test
    public void testLocateJavaScript() {
        // when and then
        IntStream.of(4, 5, 6).forEach(row -> assertEquals(
            Optional.of(new ScriptBlock(ScriptBlock.Language.JAVA, "Run method which returns 0", JAVA_SCRIPT, 0)),
            ScriptBlockUtil.locate(DEFINITION, row)));
        assertEquals(3, ScriptBlockUtil.locate(DEFINITION, 9).orElseThrow().line());
        assertEquals(5, ScriptBlockUtil.locate(DEFINITION, 11).orElseThrow().line());
    }

    @Test
    public void testLocateGroovyScript() {
        // when and then
        IntStream.of(17, 18).forEach(row -> assertEquals(
            Optional.of(new ScriptBlock(ScriptBlock.Language.GROOVY, "Run groovy script", "def run(cache) { 1 }", 0)),
            ScriptBlockUtil.locate(DEFINITION, row)));
    }

    @Test
    public void testLocateOutsideOfScripts() {
        // when and then
        IntStream.of(0, 1, 2, 3, 12, 13, 14, 15, 16, 19).forEach(row -> assertTrue(ScriptBlockUtil.locate(DEFINITION, row).isEmpty()));
    }

    @Test
    public void testLocateInInvalidDefinition() {
        // when and then
        assertTrue(ScriptBlockUtil.locate(null, 0).isEmpty());
        assertTrue(ScriptBlockUtil.locate("", 0).isEmpty());
        assertTrue(ScriptBlockUtil.locate(DEFINITION.replace("value: run", "value: [run"), 4).isEmpty());
        assertTrue(ScriptBlockUtil.locate(DEFINITION.replace("return 0;", "return 0;  "), 4).isEmpty()); // line separator
    }

    @Test
    public void testLocateSkipsAliasesAndFlowMappings() {
        // given
        final var definition = """
            templates:
              script: &script |
                def run(cache) { }
            commands:
              - name: Aliased script
                class: groovy
                inputs:
                  - name: groovy.script
                    value: *script
                  - {name: groovy.script, value: "def run(cache) { }"}
            """;

        // when and then
        IntStream.of(1, 2, 7, 8, 9).forEach(row -> assertTrue(ScriptBlockUtil.locate(definition, row).isEmpty()));
    }

    @Test
    public void testLocateScriptOfSubcommand() {
        // given
        final var definition = """
            commands:
              - name: Loop over items
                class: loop
                inputs:
                  - name: loop.items
                    value: [1, 2]
                subcommands:
                  - name: Print item
                    class: groovy
                    inputs:
                      - name: groovy.script
                        value: |
                          def run(cache) { cache.get('loop.item') }
            """;

        // when and then
        assertEquals(Optional.of(new ScriptBlock(ScriptBlock.Language.GROOVY, "Print item", "def run(cache) { cache.get('loop.item') }", 0)),
            ScriptBlockUtil.locate(definition, 12));
    }

    @Test
    public void testReplaceBlockScript() {
        // when
        final var replacement = ScriptBlockUtil.replace(DEFINITION, 9, JAVA_SCRIPT.replace("return 0;", "return 42;")).orElseThrow();

        // then
        assertEquals(5, replacement.startRow());
        assertEquals(15, replacement.startColumn());
        assertEquals(11, replacement.endRow());
        assertEquals(11, replacement.endColumn());
        assertEquals(DEFINITION.substring(DEFINITION.indexOf('|'), DEFINITION.indexOf("      - name: java.method") - 1), replacement.source());
        assertEquals(replacement.source().replace("return 0;", "return 42;"), replacement.text());
        assertEquals(DEFINITION.replace("return 0;", "return 42;"), replacement.applyTo(DEFINITION));
    }

    @Test
    public void testReplaceInlineScript() {
        // when
        final var replacement = ScriptBlockUtil.replace(DEFINITION, 17, "def run(cache) {\n  cache.put('answer', 42)\n}").orElseThrow();

        // then
        assertEquals("\"def run(cache) { 1 }\" # inline script", replacement.source());
        assertEquals(DEFINITION.replace("        value: \"def run(cache) { 1 }\" # inline script",
            "        value: | # inline script\n"
                + "          def run(cache) {\n"
                + "            cache.put('answer', 42)\n"
                + "          }"), replacement.applyTo(DEFINITION));
    }

    @Test
    public void testReplaceKeepsAnchorAndChompingIndicator() {
        // given
        final var definition = """
            commands:
              - name: Anchored script
                class: groovy
                inputs:
                  - name: groovy.script
                    value: &shared |-
                      def run(cache) {
                      }
            """;

        // when
        final var replacement = ScriptBlockUtil.replace(definition, 4, "def run(cache) {\n  1\n}").orElseThrow();

        // then
        assertEquals(definition.replace("}", "  1\n          }"), replacement.applyTo(definition));
    }

    @Test
    public void testReplaceScriptWithIndentedFirstLine() {
        // given
        final var script = "  // indented comment\nclass DynamicClass {\n}";

        // when
        final var replacement = ScriptBlockUtil.replace(DEFINITION, 4, script).orElseThrow();

        // then
        assertEquals("|2\n            // indented comment\n          class DynamicClass {\n          }", replacement.text());
        assertEquals(script, ScriptBlockUtil.locate(replacement.applyTo(DEFINITION), 4).orElseThrow().script());
    }

    @Test
    public void testReplaceFoldedScript() {
        // given
        final var definition = """
            commands:
              - name: Folded script
                class: groovy
                inputs:
                  - name: groovy.script
                    value: >
                      def run(cache) { 1 }
            """;

        // when
        final var replacement = ScriptBlockUtil.replace(definition, 4, "def run(cache) {\n  2\n}").orElseThrow();

        // then
        assertEquals("def run(cache) { 1 }", ScriptBlockUtil.locate(definition, 4).orElseThrow().script());
        assertEquals(definition.replace(">\n          def run(cache) { 1 }", "|\n          def run(cache) {\n            2\n          }"),
            replacement.applyTo(definition));
    }

    @Test
    public void testReplaceEmptyScript() {
        // given
        final var definition = """
            commands:
              - name: Empty script
                class: groovy
                inputs:
                  - name: groovy.script
                    value: |
                  - name: groovy.method
                    value: run
            """;

        // when
        final var replacement = ScriptBlockUtil.replace(definition, 5, "def run(cache) { }").orElseThrow();

        // then
        assertEquals("", ScriptBlockUtil.locate(definition, 4).orElseThrow().script());
        assertTrue(ScriptBlockUtil.locate(definition, 6).isEmpty());
        assertEquals(definition.replace("value: |", "value: |\n          def run(cache) { }"), replacement.applyTo(definition));
    }

    @Test
    public void testReplaceNormalizesBlankLines() {
        // when
        final var replacement = ScriptBlockUtil.replace(DEFINITION, 18, "def run(cache) {\n    \n  1\n}\n\n").orElseThrow();

        // then
        assertEquals("| # inline script\n          def run(cache) {\n\n            1\n          }", replacement.text());
    }

    @Test
    public void testReplaceWithWindowsLineBreaks() {
        // given
        final var definition = DEFINITION.replace("\n", "\r\n");

        // when
        final var replacement = ScriptBlockUtil.replace(definition, 9, JAVA_SCRIPT.replace("return 0;", "return 42;")).orElseThrow();

        // then
        assertEquals(DEFINITION.replace("return 0;", "return 42;"), replacement.applyTo(definition));
    }

    @Test
    public void testReplaceScriptWithSupplementaryCharacters() {
        // given
        final var definition = """
            commands:
              - name: Emoji script
                class: groovy
                inputs:
                  - name: groovy.script
                    value: "def run(cache) { '😀' }"
            """;

        // when
        final var replacement = ScriptBlockUtil.replace(definition, 4, "def run(cache) {\n  '😀'\n}").orElseThrow();

        // then
        assertEquals("\"def run(cache) { '😀' }\"", replacement.source());
        assertEquals(definition.replace("\"def run(cache) { '😀' }\"", "|\n          def run(cache) {\n            '😀'\n          }"),
            replacement.applyTo(definition));
    }

    @Test
    public void testReplaceRejectsScriptWhichCannotBeWrittenAsYaml() {
        // when and then
        assertTrue(ScriptBlockUtil.replace(DEFINITION, 4, "class DynamicClass {} ").isEmpty());
    }

    @Test
    public void testReplaceOutsideOfScripts() {
        // when and then
        assertTrue(ScriptBlockUtil.replace(DEFINITION, 12, "class DynamicClass {}").isEmpty());
    }

    @Test
    public void testLanguageOf() {
        // when and then
        assertEquals(Optional.of(ScriptBlock.Language.JAVA), ScriptBlock.Language.of("java.script"));
        assertEquals(Optional.of(ScriptBlock.Language.GROOVY), ScriptBlock.Language.of("groovy.script"));
        assertTrue(ScriptBlock.Language.of("java.method").isEmpty());
    }

}
