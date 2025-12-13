package org.myworkflows.config;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
public class SnippetConfig {

    public static final String JS_CODE = """
        const snippetManager = ace.require('ace/snippets').snippetManager;
        const yamlSnippets = [
            {
                name: 'assert',
                content: '\\n- name: ${1}\\n\\tclass: ${2:groovy|java|spel}\\n\\tvalue: ${3}',
                tabTrigger: 'assert'
            },
            {
                name: 'input',
                content: '\\n- name: ${1}\\n\\tclass: ${2:groovy|java|plain|spel}\\n\\tvalue: ${3}',
                tabTrigger: 'input'
            },
            {
                name: 'output',
                content: '\\n- name: ${1}\\n\\tclass: ${2:groovy|java|spel}\\n\\tvalue: ${3}',
                tabTrigger: 'output'
            }
        ];
        snippetManager.register(yamlSnippets, 'yaml');
        """;

}
