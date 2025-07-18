ace.define("ace/snippets/yaml.snippets",["require","exports","module"],function(e,t,n){n.exports="snippet assert\n\t- name: ${1}\n\t\tclass: ${2:groovy|java|spel}\n\t\tvalue: ${3}\nsnippet input\n\t- name: ${1}\n\t\tclass: ${2:groovy|java|plain|spel}\n\t\tvalue: ${3}\n\nsnippet output\n\t- name: ${1}\n\t\tclass: ${2:groovy|java|spel}\n\t\tvalue: ${3}\n"}),ace.define("ace/snippets/yaml",["require","exports","module","ace/snippets/yaml.snippets"],function(e,t,n){"use strict";t.snippetText=e("./yaml.snippets"),t.scope="yaml"});
(function () {
    ace.require(["ace/snippets/yaml"], function (m) {
        if (typeof module == "object" && typeof exports == "object" && module) {
            module.exports = m;
        }
    });
})();