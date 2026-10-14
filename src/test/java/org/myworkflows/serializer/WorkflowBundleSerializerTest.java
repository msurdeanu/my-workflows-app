package org.myworkflows.serializer;

import org.junit.jupiter.api.Test;
import org.myworkflows.domain.WorkflowParameterType;
import org.myworkflows.domain.command.UnknownCommand;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.domain.transfer.WorkflowBundleFormat;
import org.myworkflows.exception.WorkflowTransferException;
import org.myworkflows.exception.WorkflowTransferException.Reason;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.SLEEP_SCRIPT;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.requestScript;
import static org.myworkflows.domain.transfer.WorkflowTransferFixture.script;

/**
 * @author Mihai Surdeanu
 * @since 2.1
 */
public final class WorkflowBundleSerializerTest {

    private static final String EXPECTED_YAML = """
        format: 1
        exported:
          at: 2026-10-13T09:42:00Z
          by: admin
          version: 2.1
        placeholders:
        - name: SMTP.HOST
          value: smtp.prod.local
        - name: API.PASSWORD
        parameters:
        - name: recipients
          type: EMAIL
          value: ops@example.com
        - name: smtpPassword
          type: PASS
        definitions:
        - name: Ping services
          script:
            commands:
            - class: httpRequest
              name: Ask
              inputs:
              - name: httpRequest.url
                value: $$(API.URL)/health
        templates:
        - name: Health check
          cron: 0 */10 * * * *
          enabled: true
          definitions:
          - Ping services
          parameters:
          - recipients
          ownParameters:
          - name: timeout
            type: INT
            value: 60
        - name: Paused
          enabled: false
          definitions: []
          parameters: []
        """;

    @Test
    public void whenBundleIsWrittenAsOneFileThenItReadsAsTheFileSaysAndComesBackTheSame() {
        // given
        final var bundle = createBundle();

        // when
        final var content = WorkflowBundleSerializer.toBytes(bundle, WorkflowBundleFormat.YAML);

        // then
        assertEquals(EXPECTED_YAML, new String(content, UTF_8));
        final var readBundle = WorkflowBundleSerializer.toBundle(content);
        assertEquals(bundle.exported(), readBundle.exported());
        assertEquals(bundle.placeholders(), readBundle.placeholders());
        assertEquals(bundle.parameters(), readBundle.parameters());
        assertEquals(bundle.templates(), readBundle.templates());
        // a value which was left out is still missing, which is not the same as being empty
        assertNull(readBundle.placeholders().get(1).value());
        assertArrayEquals(content, WorkflowBundleSerializer.toBytes(readBundle, WorkflowBundleFormat.YAML));
        assertEquals(8, readBundle.size());
    }

    @Test
    public void whenBundleIsWrittenAsAnArchiveThenItHoldsAFilePerDefinitionAndPerTemplate() throws IOException {
        // given
        final var bundle = createBundle();

        // when
        final var content = WorkflowBundleSerializer.toBytes(bundle, WorkflowBundleFormat.ZIP);

        // then
        final var files = unzip(content);
        assertEquals(List.of("manifest.yaml", "placeholders.yaml", "parameters.yaml", "definitions/ping-services.yaml", "templates/health-check.yaml",
            "templates/paused.yaml"), List.copyOf(files.keySet()));
        // a definition is its script and nothing else, so the manifest keeps its name
        assertEquals(requestScript("$$(API.URL)/health"), files.get("definitions/ping-services.yaml"));
        assertEquals("""
            format: 1
            exported:
              at: 2026-10-13T09:42:00Z
              by: admin
              version: 2.1
            definitions:
            - name: Ping services
              file: definitions/ping-services.yaml
            templates:
            - name: Health check
              file: templates/health-check.yaml
            - name: Paused
              file: templates/paused.yaml
            """, files.get("manifest.yaml"));
        assertTrue(files.get("templates/health-check.yaml").startsWith("name: Health check\n"));

        // and it reads back as what the single file holds
        final var readBundle = WorkflowBundleSerializer.toBundle(content);
        assertEquals(EXPECTED_YAML, new String(WorkflowBundleSerializer.toBytes(readBundle, WorkflowBundleFormat.YAML), UTF_8));
    }

    @Test
    public void whenTwoNamesComeDownToTheSameFileThenTheFilesAreToldApartByANumber() throws IOException {
        // given names which a file cannot be named after as they are
        final var bundle = new WorkflowBundle(1, null, List.of(), List.of(),
            List.of(new WorkflowBundle.Definition("Ping services", script(SLEEP_SCRIPT)), new WorkflowBundle.Definition("ping: services!", script(SLEEP_SCRIPT)),
                new WorkflowBundle.Definition("../../etc/passwd", script(SLEEP_SCRIPT)), new WorkflowBundle.Definition("???", script(SLEEP_SCRIPT))),
            List.of());

        // when
        final var content = WorkflowBundleSerializer.toBytes(bundle, WorkflowBundleFormat.ZIP);

        // then no placeholders and no parameters means no file for them, and every definition has a file of its own
        assertEquals(List.of("manifest.yaml", "definitions/ping-services.yaml", "definitions/ping-services-2.yaml", "definitions/etc-passwd.yaml",
            "definitions/item.yaml"), List.copyOf(unzip(content).keySet()));
        assertEquals(List.of("Ping services", "ping: services!", "../../etc/passwd", "???"),
            WorkflowBundleSerializer.toBundle(content).definitions().stream().map(WorkflowBundle.Definition::name).toList());
    }

    @Test
    public void whenValueLooksLikeSomethingElseThanATextThenItIsStillReadAsTheTextItWas() {
        // given values which YAML would take for a number, a literal, a comment or nothing at all
        final var values = List.of("007", "1e3", "0x1F", "true", "null", "~", "", " padded ", "a: b", "# no comment", "- no item", "two\nlines",
            "2026-01-31", "yes", "'quoted'", "\"double\"", "{not: a map}", "$$(NAME)", "tab\there");
        final var placeholders = new ArrayList<WorkflowBundle.Placeholder>();
        for (int index = 0; index < values.size(); index++) {
            placeholders.add(new WorkflowBundle.Placeholder("VALUE." + index, values.get(index)));
        }
        final var bundle = new WorkflowBundle(1, null, placeholders, List.of(), List.of(), List.of());

        // when and then
        for (var format : WorkflowBundleFormat.values()) {
            final var readBundle = WorkflowBundleSerializer.toBundle(WorkflowBundleSerializer.toBytes(bundle, format));
            assertEquals(values, readBundle.placeholders().stream().map(WorkflowBundle.Placeholder::value).toList());
        }
    }

    @Test
    public void whenDefinitionUsesAnUnknownCommandThenItIsKeptAsItWasWritten() {
        // given a file from an instance which has a library this one does not have
        final var yaml = """
            format: 1
            definitions:
            - name: Archive
              script:
                commands:
                - class: s3Upload
                  name: Upload
                  inputs:
                  - name: s3Upload.bucket
                    value: reports
            """;

        // when
        final var bundle = WorkflowBundleSerializer.toBundle(yaml.getBytes(UTF_8));

        // then
        assertNull(bundle.exported());
        assertTrue(bundle.placeholders().isEmpty());
        assertTrue(bundle.templates().isEmpty());
        final var command = assertInstanceOf(UnknownCommand.class, bundle.definitions().getFirst().script().getCommands().getFirst());
        assertEquals("s3Upload", command.getType());
        assertEquals(yaml + "templates: []\n", new String(WorkflowBundleSerializer.toBytes(bundle, WorkflowBundleFormat.YAML), UTF_8)
            .replace("placeholders: []\nparameters: []\n", ""));
    }

    @Test
    public void whenFileStartsWithAByteOrderMarkThenItIsStillRead() {
        // when
        final var bundle = WorkflowBundleSerializer.toBundle(("﻿" + EXPECTED_YAML).getBytes(UTF_8)); // the mark an editor may write first

        // then
        assertEquals("Ping services", bundle.definitions().getFirst().name());
    }

    @Test
    public void whenFileIsNoExportThenItIsRefusedWithTheReason() throws IOException {
        // a text which is no YAML
        assertReason(Reason.FILE_UNREADABLE, "format: [1".getBytes(UTF_8));
        assertReason(Reason.FILE_UNREADABLE, "format: 1\nparameters:\n- name: x\n  type: NOPE\n".getBytes(UTF_8));
        assertReason(Reason.FILE_UNREADABLE, new byte[0]);
        // YAML which is something else, like the script of a definition
        assertReason(Reason.FILE_NOT_AN_EXPORT, SLEEP_SCRIPT.getBytes(UTF_8));
        assertReason(Reason.FILE_NOT_AN_EXPORT, "~".getBytes(UTF_8));
        // an export of a version which came later
        final var exception = assertReason(Reason.FORMAT_NOT_SUPPORTED, "format: 99".getBytes(UTF_8));
        assertArrayEquals(new Object[] {99, WorkflowBundle.FORMAT}, exception.getArguments());
        // an archive which is something else
        assertReason(Reason.FILE_NOT_AN_EXPORT, zip(Map.of("readme.txt", "hello")));
        // and one which lost a file
        final var incomplete = assertReason(Reason.FILE_INCOMPLETE,
            zip(Map.of("manifest.yaml", "format: 1\ndefinitions:\n- name: Gone\n  file: definitions/gone.yaml\n")));
        assertArrayEquals(new Object[] {"definitions/gone.yaml"}, incomplete.getArguments());
        assertReason(Reason.FILE_UNREADABLE, zip(Map.of("manifest.yaml", "format: [1")));
    }

    @Test
    public void whenArchiveHasNoListOfItsOwnThenWhatItListsIsAllThereIs() throws IOException {
        // given an archive with a manifest alone, and a file nothing names
        final var content = zip(Map.of("manifest.yaml", "format: 1\n", "definitions/stray.yaml", SLEEP_SCRIPT));

        // when
        final var bundle = WorkflowBundleSerializer.toBundle(content);

        // then
        assertEquals(0, bundle.size());
        assertFalse(bundle.format() != 1);
    }

    private WorkflowTransferException assertReason(Reason reason, byte[] content) {
        final var exception = assertThrows(WorkflowTransferException.class, () -> WorkflowBundleSerializer.toBundle(content));
        assertEquals(reason, exception.getReason());
        return exception;
    }

    private WorkflowBundle createBundle() {
        return new WorkflowBundle(WorkflowBundle.FORMAT, new WorkflowBundle.Origin(Instant.parse("2026-10-13T09:42:00Z"), "admin", "2.1"),
            List.of(new WorkflowBundle.Placeholder("SMTP.HOST", "smtp.prod.local"), new WorkflowBundle.Placeholder("API.PASSWORD", null)),
            List.of(new WorkflowBundle.Parameter("recipients", WorkflowParameterType.EMAIL, "ops@example.com"),
                new WorkflowBundle.Parameter("smtpPassword", WorkflowParameterType.PASS, null)),
            List.of(new WorkflowBundle.Definition("Ping services", script(requestScript("$$(API.URL)/health")))),
            List.of(new WorkflowBundle.Template("Health check", "0 */10 * * * *", true, List.of("Ping services"), List.of("recipients"),
                    List.of(new WorkflowBundle.Parameter("timeout", WorkflowParameterType.INT, "60"))),
                new WorkflowBundle.Template("Paused", null, false, List.of(), List.of(), List.of())));
    }

    private Map<String, String> unzip(byte[] content) throws IOException {
        final var files = new LinkedHashMap<String, String>();
        try (var archive = new ZipInputStream(new ByteArrayInputStream(content), UTF_8)) {
            ZipEntry entry;
            while ((entry = archive.getNextEntry()) != null) {
                files.put(entry.getName(), new String(archive.readAllBytes(), UTF_8));
            }
        }
        return files;
    }

    private byte[] zip(Map<String, String> files) throws IOException {
        final var buffer = new ByteArrayOutputStream();
        try (var archive = new ZipOutputStream(buffer, UTF_8)) {
            for (var file : files.entrySet()) {
                archive.putNextEntry(new ZipEntry(file.getKey()));
                archive.write(file.getValue().getBytes(UTF_8));
                archive.closeEntry();
            }
        }
        return buffer.toByteArray();
    }

}
