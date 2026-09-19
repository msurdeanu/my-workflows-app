package org.myworkflows.view.util;

import de.f0rce.ace.enums.AceMode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.myworkflows.domain.command.GroovyCommand;
import org.myworkflows.domain.command.JavaCommand;

import java.util.Arrays;
import java.util.Optional;

/**
 * A script found inside a workflow definition, i.e. the value of a java.script or groovy.script input. The line is the
 * script line that matches the definition row used to locate the script.
 *
 * @author Mihai Surdeanu
 * @since 1.3
 */
public record ScriptBlock(Language language, String commandName, String script, int line) {

    /**
     * @author Mihai Surdeanu
     * @since 1.3
     */
    @Getter
    @RequiredArgsConstructor
    public enum Language {
        JAVA(JavaCommand.PREFIX, AceMode.java),
        GROOVY(GroovyCommand.PREFIX, AceMode.groovy);

        private final String prefix;
        private final AceMode mode;

        public static Optional<Language> of(String inputName) {
            return Arrays.stream(values())
                .filter(language -> language.getInputName().equals(inputName))
                .findFirst();
        }

        public String getInputName() {
            return prefix + ".script";
        }

    }

}
