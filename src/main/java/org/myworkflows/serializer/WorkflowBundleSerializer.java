package org.myworkflows.serializer;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.myworkflows.domain.WorkflowDefinitionScript;
import org.myworkflows.domain.transfer.WorkflowBundle;
import org.myworkflows.domain.transfer.WorkflowBundleFormat;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.myworkflows.exception.WorkflowTransferException;
import org.myworkflows.exception.WorkflowTransferException.Reason;
import org.springframework.core.NestedExceptionUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Optional.ofNullable;

/**
 * Writes a {@link WorkflowBundle} to a file and reads it back, in either {@link WorkflowBundleFormat}. An archive holds
 * what the single text holds, cut into a file per definition - its script alone, as the editor shows it - and per
 * template, next to a manifest which keeps the name behind every file.
 *
 * <p>A command whose type is not known where a file is read is kept as it was written: a file is read on another
 * instance most of the time, which may not have the libraries of the one it comes from.</p>
 *
 * @author Mihai Surdeanu
 * @since 2.1
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class WorkflowBundleSerializer {

    private static final String MANIFEST_FILE = "manifest.yaml";
    private static final String PLACEHOLDERS_FILE = "placeholders.yaml";
    private static final String PARAMETERS_FILE = "parameters.yaml";
    private static final String DEFINITIONS_DIRECTORY = "definitions/";
    private static final String TEMPLATES_DIRECTORY = "templates/";
    private static final String FILE_EXTENSION = ".yaml";

    // what an archive may unpack to: far more than an export ever is, and little enough to be held in memory
    private static final int MAX_ENTRIES = 5_000;
    private static final long MAX_SIZE = 64L * 1024 * 1024;

    private static final int MAX_MESSAGE_LENGTH = 300;

    private static final char BYTE_ORDER_MARK = '﻿'; // what some editors start a text with, which is no part of it

    public static byte[] toBytes(WorkflowBundle bundle, WorkflowBundleFormat format) {
        return format == WorkflowBundleFormat.ZIP ? toArchive(bundle) : write(bundle).getBytes(UTF_8);
    }

    /**
     * @param content a file as {@link #toBytes(WorkflowBundle, WorkflowBundleFormat)} writes it. Which of the two formats
     *     it has is told from what it starts with, not from its name.
     */
    public static WorkflowBundle toBundle(byte[] content) {
        final var bundle = isArchive(content) ? fromArchive(content) : read(toText(content), WorkflowBundle.class);
        // any YAML which is not a list reads as a bundle without a thing in it, as nothing of a bundle is required
        if (bundle == null || bundle.format() < 1) {
            throw new WorkflowTransferException(Reason.FILE_NOT_AN_EXPORT);
        }
        if (bundle.format() > WorkflowBundle.FORMAT) {
            throw new WorkflowTransferException(Reason.FORMAT_NOT_SUPPORTED, bundle.format(), WorkflowBundle.FORMAT);
        }
        return bundle;
    }

    private static byte[] toArchive(WorkflowBundle bundle) {
        final var definitionFiles = nameFiles(DEFINITIONS_DIRECTORY, bundle.definitions().stream().map(WorkflowBundle.Definition::name).toList());
        final var templateFiles = nameFiles(TEMPLATES_DIRECTORY, bundle.templates().stream().map(WorkflowBundle.Template::name).toList());
        final var buffer = new ByteArrayOutputStream();
        try (var archive = new ZipOutputStream(buffer, UTF_8)) {
            addEntry(archive, MANIFEST_FILE, new Manifest(bundle.format(), bundle.exported(), definitionFiles, templateFiles));
            if (!bundle.placeholders().isEmpty()) {
                addEntry(archive, PLACEHOLDERS_FILE, bundle.placeholders());
            }
            if (!bundle.parameters().isEmpty()) {
                addEntry(archive, PARAMETERS_FILE, bundle.parameters());
            }
            for (int index = 0; index < definitionFiles.size(); index++) {
                addEntry(archive, definitionFiles.get(index).file(), bundle.definitions().get(index).script());
            }
            for (int index = 0; index < templateFiles.size(); index++) {
                addEntry(archive, templateFiles.get(index).file(), bundle.templates().get(index));
            }
        } catch (IOException exception) {
            throw new WorkflowTransferException(Reason.EXPORT_FAILED, exception);
        }
        return buffer.toByteArray();
    }

    private static void addEntry(ZipOutputStream archive, String file, Object content) throws IOException {
        archive.putNextEntry(new ZipEntry(file));
        archive.write(write(content).getBytes(UTF_8));
        archive.closeEntry();
    }

    private static String write(Object content) {
        return ofNullable(SerializerFactory.toString(content, null)).orElseThrow(() -> new WorkflowTransferException(Reason.EXPORT_FAILED));
    }

    /**
     * @return a file for each name, in the given directory. The file is named after what it holds, as far as a name can
     *     be the name of a file: two names which come down to the same file are told apart by a number.
     */
    private static List<Entry> nameFiles(String directory, List<String> names) {
        final var usedFiles = new HashSet<String>();
        final var entries = new ArrayList<Entry>(names.size());
        names.forEach(name -> {
            final var slug = StringUtils.defaultIfEmpty(StringUtils.strip(
                StringUtils.stripAccents(StringUtils.defaultString(name)).toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]+", "-"), "-"), "item");
            var file = slug;
            for (int number = 2; !usedFiles.add(file); number++) {
                file = slug + "-" + number;
            }
            entries.add(new Entry(name, directory + file + FILE_EXTENSION));
        });
        return entries;
    }

    private static WorkflowBundle fromArchive(byte[] content) {
        final var files = readFiles(content);
        if (!files.containsKey(MANIFEST_FILE)) {
            throw new WorkflowTransferException(Reason.FILE_NOT_AN_EXPORT);
        }
        final var manifest = read(toText(files.get(MANIFEST_FILE)), Manifest.class);
        if (manifest == null) {
            throw new WorkflowTransferException(Reason.FILE_NOT_AN_EXPORT);
        }

        final var definitions = manifest.definitions().stream()
            .map(entry -> new WorkflowBundle.Definition(entry.name(), read(findText(files, entry.file()), WorkflowDefinitionScript.class)))
            .toList();
        final var templates = manifest.templates().stream()
            .map(entry -> read(findText(files, entry.file()), WorkflowBundle.Template.class))
            .toList();
        return new WorkflowBundle(manifest.format(), manifest.exported(), readList(files, PLACEHOLDERS_FILE, WorkflowBundle.Placeholder[].class),
            readList(files, PARAMETERS_FILE, WorkflowBundle.Parameter[].class), definitions, templates);
    }

    private static Map<String, byte[]> readFiles(byte[] content) {
        final var files = new LinkedHashMap<String, byte[]>();
        long size = 0;
        try (var archive = new ZipInputStream(new ByteArrayInputStream(content), UTF_8)) {
            ZipEntry entry;
            while ((entry = archive.getNextEntry()) != null) {
                if (entry.isDirectory()) {
                    continue;
                }
                // one byte more than what is left is asked for: getting it is what tells that the archive is too large
                final var file = archive.readNBytes((int) (MAX_SIZE - size + 1));
                size += file.length;
                if (size > MAX_SIZE || files.size() >= MAX_ENTRIES) {
                    throw new WorkflowTransferException(Reason.FILE_TOO_LARGE);
                }
                files.put(entry.getName().replace('\\', '/'), file);
            }
        } catch (IOException | IllegalArgumentException exception) {
            throw new WorkflowTransferException(Reason.FILE_UNREADABLE, exception, describe(exception));
        }
        return files;
    }

    private static <T> List<T> readList(Map<String, byte[]> files, String file, Class<T[]> type) {
        return ofNullable(files.get(file))
            .map(content -> read(toText(content), type))
            .map(Arrays::asList)
            .orElse(List.of());
    }

    private static String findText(Map<String, byte[]> files, String file) {
        return ofNullable(file)
            .map(files::get)
            .map(WorkflowBundleSerializer::toText)
            .orElseThrow(() -> new WorkflowTransferException(Reason.FILE_INCOMPLETE, String.valueOf(file)));
    }

    private static <T> T read(String text, Class<T> type) {
        try {
            return SerializerFactory.toObjectKeepingUnknownCommands(text, type);
        } catch (WorkflowRuntimeException exception) {
            throw new WorkflowTransferException(Reason.FILE_UNREADABLE, exception, describe(exception));
        }
    }

    private static String toText(byte[] content) {
        final var text = new String(content, UTF_8);
        return !text.isEmpty() && text.charAt(0) == BYTE_ORDER_MARK ? text.substring(1) : text;
    }

    /**
     * An archive starts with the two letters its format was named after.
     */
    private static boolean isArchive(byte[] content) {
        return content.length > 1 && content[0] == 'P' && content[1] == 'K';
    }

    /**
     * @return why a file could not be read, cut down to what somebody can do something about: where a parser gives up,
     *     it goes on to quote what it was reading.
     */
    private static String describe(Exception exception) {
        final var cause = ofNullable(NestedExceptionUtils.getRootCause(exception)).orElse(exception);
        return StringUtils.abbreviate(StringUtils.substringBefore(StringUtils.defaultString(cause.getMessage()), "\n"), MAX_MESSAGE_LENGTH);
    }

    /**
     * What an archive holds besides the items themselves.
     *
     * @param definitions the definitions of the archive: a definition is a file which holds its script and nothing
     *     else, so its name is kept here.
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    record Manifest(int format, WorkflowBundle.Origin exported, List<Entry> definitions, List<Entry> templates) {

        Manifest {
            definitions = ofNullable(definitions).orElse(List.of());
            templates = ofNullable(templates).orElse(List.of());
        }

    }

    /**
     * @author Mihai Surdeanu
     * @since 2.1
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    record Entry(String name, String file) {

    }

}
