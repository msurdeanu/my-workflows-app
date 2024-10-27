# MyWorkflows

[MyWorkflows](https://myworkflows.org) is a simple tool for creating workflows to automate daily manual tasks.
You can easily create workflows, schedule them using cron-jobs and see their output.

Uses Java as programming language and Vaadin as UI framework.

This tool is perfect for small teams which are flooded by manual tasks day by day.
All workflows are defined in JSON format and for playing with them, you have a nice UI.

## Technology stack

* Java 21 as programming language.
* Spring Boot 3.x as dependency injection framework.
* [Vaadin 24](https://vaadin.com) as UI framework.
* [SQLite](https://www.sqlite.org/) as relational database for persisting data.
* [JSON Schema Validator](https://github.com/networknt/json-schema-validator) as JSON schema validator for workflow
  scripts.
* [Groovy](https://groovy-lang.org/) as additional language for defining commands
* [Janino](https://www.janino.net/) as Java runtime compiler.
* [SpEL](https://docs.spring.io/spring-framework/docs/3.0.x/reference/expressions.html) as another runtime evaluator.

## Features

* **Modern** and **responsive** UI.
* **Authentication** and **authorization** enabled by default.
* Vaadin **Push** enabled. Through websockets, the server can send updates to the client.
* Persistence layer based on **SQLite**.
* Dedicated page for **workflow definition script development**, from where you can code your script, inject parameters,
  run the script and see the print in real time. The editor has auto complete to make your life easier.
* Dedicated web pages for **workflow definitions**, **workflow templates** and **workflow runs**.
* Dedicated web page for writing **markdown documentation**.
* Dedicated web page for **statistics**.

## TODOs

- [ ] **Checkpoints**. Introduce ability to reschedule a failing workflow from a specific moment in time.
- [ ] **Debug mode**. Improve debugging experience for failing workflows to easily find the root cause of their failure.

## How it works?

### Abbreviations

* **Workflow**: Represents a list of steps to resolve a given task.
* **Command**: Represents a unitary step in the workflow. A command has a `name`, a `@type`, `ifs`, `inputs`,
  `asserts` and `outputs`. `name` and `@type` are mandatory for being able to define a valid command.
* **If**: Each command allows to define some running conditions. If at least one condition is not met, the command will
  be skipped.
* **Input**: Each command allows input parameters to customize the running step.
* **Assert**: Once a command is run, if the command output exists, you can inject different assertions.
* **Output**: Once a command is run, if the command output exists, you can do other processing (like saving the output
  in another variable)
* **Variable**: Represents a standalone data variable which is generated by a command and can be used as input by
  upcoming commands.

### Workflow

`Workflow` is the logic entity that encapsulates multiple steps - called `commands` - needed to implement a task.
Each workflow has a `name` to be able to identify easily what is doing.

When the time comes, the workflow can be run `manually` by the user or `automatically` using a scheduler.
Behind the scene, there is a `thread pool` responsible for executing the workflow.
Each workflow is scheduled to run inside a single thread and all his commands are run in sequential order.

### Features

#### Finally commands

The concept of `finally commands` lets you declare some special commands for your workflow to be executed at the end, no
matter if the workflow failed or not.
The concept is similar with `try-finally` block functionality from Java.

```
try { 
  // Run all commands in a sequential order
} finally {
  // Run all finallyCommands in a sequential order
}
```

> [!TIP]
> You are not allowed to have more than 10 finally commands inside one workflow definition!

#### Placeholders

You are allowed to use global `placeholders` to avoid data duplication in your workflow definition.
Please note that all placeholders are resolved immediately before workflow running process.
All placeholders are persisted in a database - a table called `placeholders`.

Inside workflow definition, you can recognize a placeholder by having this format: `$$(PLACEHOLDER_NAME)`.
You are allowed to use placeholders inside any `input`, `assert` and `output` (`name` and `value` fields).

> [!TIP]
> The name of the placeholder should match pattern `[a-zA-Z0-9_]+` for being accepted as a valid placeholder.

> [!TIP]
> If the placeholder that you are looking for is not found, you are going to receive a runtime exception.

#### Expressions

Expressions are the heart of this tool.
By using them, you will be able to pass information between commands.
Expressions are evaluated at runtime, and they can be used anywhere in an `input`, `assert`, `output` and `if`structure.
Inside this structure, the expression will always be defined inside `value` field.
Each expression is evaluated by a runtime evaluator specified by the user, after filling `@type` field.

For the moment, there are 3 runtime evaluators supported:

1. set `@type` to `groovy` if you want to enable Groovy runtime evaluator.
2. set `@type` to `java` if you want to enable Java runtime evaluator based on [Janino](https://www.janino.net/) runtime
   compiler.
3. set `@type` to `spel` if you want to
   enable [SpEL](https://docs.spring.io/spring-framework/docs/3.0.x/reference/expressions.html) runtime evaluator.

##### Examples

###### Retrieve exit code after running sshExec command by using SpEL runtime evaluator

```json
{
  "name": "Asserts exitCode to be equal with 0",
  "@type": "spel",
  "value": "#output.getExitCode() == 0"
}
```

###### Retrieve information from workflow run cache and save it to another variable (named `sleep.time`)

```json
{
  "name": "sleep.time",
  "@type": "groovy",
  "value": "cache.get('sleepTime').toInteger()"
}
```

##### Cache access patterns

If you analyze all the above examples, you are going to see that sometimes is quite difficult to write the expression,
because is too long. In addition, by accessing the workflow run cache using the following pattern
`cache.get('sleepTime')`, it makes the expression harder to be read. Basically, it's not human-readable ...

This is why, you can use a simplified version by using **cache access pattern feature** exposed by this tool.

If we take one of the previous examples:

```json
{
  "name": "sleep.time",
  "@type": "groovy",
  "value": "cache.get('sleepTime').toInteger()"
}
```

we can rewrite it like this:

```json
{
  "name": "sleep.time",
  "@type": "groovy",
  "value": "$(sleepTime:Integer.class)"
}
```

The tool will be able to recognize every string pattern inside `value` which matches the following regex pattern:
`\$\(([a-zA-Z0-9_.]+)(:[a-zA-Z0-9_.]+)?\)`. The string between parenthesis is split in two parts: first part defines
the name of the variable that we are going to search into workflow run cache (as you can see this is mandatory) and
the second part is optional and defines the type of the value that we are looking for.

If there is no variable named `sleepTime` of type `Integer` in the workflow run cache, during workflow execution phase,
you are going to receive a runtime exception.

#### JAR loading at runtime

The tool is capable of loading a list of JAR files at runtime, during application initialization phase.
This is quite useful if you want to extend Java or Groovy commands with more functionality.

In order to do this, please use the following application config property:

```yaml
my-workflows:
  config:
    loader:
      jars:
        - "/home/admin/file1.jar" # Full path is recommended
        - "/home/admin/file2.jar"
```

> [!TIP]
> Once you change this property, your application has to be restarted!

#### Comments

From technical point of view, each workflow is defined in JSON format.
If you are familiar with JSON, you probably know that comments are not allowed.
Since comments are useful sometimes, there is a hack that can be implemented to have this wonderful feature: by using a
dedicated field called `_comment.*`.
This field can be used anywhere in the workflow definition.

## Predefined type of commands

### Groovy command

Provides ability to run Groovy code at runtime.
As you probably already imagine, this command is very powerful.

| `@type`  | Inputs                                                                                                                                                                                                                                                                          | Output                             |
|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------|
| `groovy` | <ul><li><strong>java.scriptLines</strong>: Mandatory. Represents source code which contains definition of a `java.methodName` (or `run`) method to be executed.</li><li><em>java.methodName</em>: Optional. Represents the method name invoked when code is executed.</li></ul> | Return of invoked method or `void` |

Example of a dummy command:

```json
{
  "name": "Run empty method",
  "@type": "groovy",
  "inputs": [
    {
      "name": "java.scriptLines",
      "value": [
        "def run(workflowRunCache) {",
        "}"
      ]
    },
    {
      "name": "java.methodName",
      "value": "run"
    }
  ]
}
```

### Java command

Provides ability to run Java code at runtime.
Like for Groovy command, this command is also very powerful.

| `@type` | Inputs                                                                                                                                                                                                                                                                                                                                                                                                                                          | Output                             |
|---------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------|
| `java`  | <ul><li><strong>java.scriptLines</strong>: Mandatory. Represents source code which contains definition of a `java.methodName` (or `run`) method inside a class `java.className` (or `DynamicClass`) which will be executed.</li><li><em>java.methodName</em>: Optional. Represents the method name invoked when code is executed.</li><li><em>java.className</em>: Optional. Represents the class name invoked when code is executed.</li></ul> | Return of invoked method or `void` |

Example of a dummy command:

```json
{
  "name": "Run method which returns 0",
  "@type": "java",
  "inputs": [
    {
      "name": "java.scriptLines",
      "value": [
        "import org.myworkflows.domain.WorkflowRunCache;",
        "public class DynamicClass {",
        "  public int run(WorkflowRunCache cache) {",
        "    return 0;",
        "  }",
        "}"
      ]
    },
    {
      "name": "java.methodName",
      "value": "run"
    },
    {
      "name": "java.className",
      "value": "DynamicClass"
    }
  ]
}
```

### Nothing command

This command is not doing anything.
The purpose of this command is to allow inputs to be injected in the workflow pipeline.

| `@type`   | Inputs | Output |
|-----------|--------|--------|
| `nothing` | N/A    | N/A    |

Example of a dummy command:

```json
{
  "name": "Inject input parameters or process existing ones",
  "@type": "nothing",
  "inputs": [
    {
      "name": "test",
      "value": "Just a simple test"
    }
  ]
}
```

### Print command

Captures an input / output variable during workflow execution and shows the value to the UI.

| `@type` | Inputs                                                                                                                | Output                                                                |
|---------|-----------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------|
| `print` | <ul><li><strong>print.keys</strong>: Mandatory. Represents a list of variable names that will be displayed.</li></ul> | Returns total number of keys affected by this operation. Type: `int`. |

Example of a dummy command:

```json
{
  "name": "Print 'commandOutput' value",
  "@type": "print",
  "inputs": [
    {
      "name": "print.keys",
      "value": [
        "commandOutput"
      ]
    }
  ]
}
```

> [!IMPORTANT]  
> All variables which contains in their name `password` (case-insensitive) will have all chars replaced with `*`.

### Sleep command

Provides ability to pause current workflow execution by a given time.
Time unit is milliseconds.

| `@type` | Inputs                                                                                               | Output                                       |
|---------|------------------------------------------------------------------------------------------------------|----------------------------------------------|
| `sleep` | <ul><li><strong>sleep.time</strong>: Mandatory. Represents number of millis used to sleep.</li></ul> | Returns the actual time slept. Type: `long`. |

Example of a dummy command:

```json
{
  "name": "Sleep for one second",
  "@type": "sleep",
  "inputs": [
    {
      "name": "sleep.time",
      "value": 1000
    }
  ]
}
```

## Examples of workflow definition scripts

### Make an HTTP GET request, check status code to be OK, extract page body into an variable and print it

```json
{
  "commands": [
    {
      "@type": "httpRequest",
      "name": "Make HTTP request",
      "inputs": [
        {
          "name": "httpRequest.url",
          "value": "https://aventurata.ro/wp-json/wp/v2"
        }
      ],
      "asserts": [
        {
          "name": "Assert request status code",
          "value": "output.getStatusCode() == org.springframework.http.HttpStatus.OK",
          "@type": "java"
        }
      ],
      "outputs": [
        {
          "name": "requestBody",
          "value": "output.getBody()",
          "@type": "java"
        }
      ]
    },
    {
      "@type": "print",
      "name": "Print requestBody",
      "inputs": [
        {
          "name": "print.keys",
          "value": [
            "requestBody"
          ]
        }
      ]
    }
  ]
}
```

### Custom Java command that parses a JSON string based on json-path library

This example loads JAR files from disk during bootstrap phase:

```yaml
my-workflows:
  config:
    loader:
      jars:
        - accessors-smart-2.5.0.jar
        - json-smart-2.5.0.jar
        - json-path-2.9.0.jar
```

```json
{
  "commands": [
    {
      "name": "Parse JSON using json-path library",
      "@type": "java",
      "inputs": [
        {
          "name": "java.scriptLines",
          "value": [
            "import com.jayway.jsonpath.JsonPath;",
            "import org.myworkflows.domain.WorkflowRunCache;",
            "public class DynamicClass {",
            "  public Object run(WorkflowRunCache cache) {",
            "    return JsonPath.parse(\"[{'a':1,'b':2}]\").read(\"$[0]['b']\");",
            "  }",
            "}"
          ]
        }
      ],
      "outputs": [
        {
          "name": "test",
          "value": "output",
          "@type": "java"
        }
      ]
    },
    {
      "@type": "print",
      "name": "Print placeholder value",
      "inputs": [
        {
          "name": "print.keys",
          "value": [
            "test"
          ]
        }
      ]
    }
  ]
}
```

## From development perspective

### Running the application

There are two ways to run the application: using `mvn spring-boot:run` or by running the `Application` class directly
from your IDE.

You can use any IDE of your preference, but I suggest IntelliJ IDEA.

### Application bootstrap phase

During application bootstrap phase, multiple local caches are filled with information found in database.
The data is loaded in the following order:

1. Once the **application is ready**, the app will load any kind of **external JAR** provided by the user.
2. After all **external JARs** are loaded, the app will proceed with the **placeholders**. All of them are loaded from
   database.
3. Once all **placeholders** are loaded, the app will continue with **workflow definitions**. Again, all of them are
   loaded from database.
4. When all **workflow definitions** are available, the app will load all **workflow templates** found in database. They
   are also scheduled.
5. Next step is to load all **workflow runs**. In fact, is not all of them. The app will load only the
   accepted capacity for workflow runs defined in application config.
6. Last but not least, the app will load all documentation pages available in the local database.

### Structure

Vaadin web applications are full-stack and include both client-side and server-side code in the same project.

| Directory                                                    | Description                                           |
|:-------------------------------------------------------------|:------------------------------------------------------|
| `frontend/`                                                  | Client-side source directory                          |
| &nbsp;&nbsp;&nbsp;&nbsp;`themes/`                            | Themes directory (CSS)                                |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`simple/`    | Default theme                                         |
| `src/main/java/org/myworkflows`                              | Server-side source directory                          |
| &nbsp;&nbsp;&nbsp;&nbsp;`cache/`                             | Contains a custom cache implementation                |
| &nbsp;&nbsp;&nbsp;&nbsp;`config/`                            | Package with different Spring configuration beans     |
| &nbsp;&nbsp;&nbsp;&nbsp;`domain/`                            | Package with all classes part of application domain   |
| &nbsp;&nbsp;&nbsp;&nbsp;`holder/`                            | Contains a set of holders to store shared data        |
| &nbsp;&nbsp;&nbsp;&nbsp;`provider/`                          | Java package with a set of providers                  |
| &nbsp;&nbsp;&nbsp;&nbsp;`repository/`                        | Contains a set of JpaRepositories                     |
| &nbsp;&nbsp;&nbsp;&nbsp;`service/`                           | Contains a set of services exposed by the application |
| &nbsp;&nbsp;&nbsp;&nbsp;`view/`                              | Contains a set of views exposed by the application    |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`component/` | Package with all Vaadin custom components             |
| &nbsp;&nbsp;&nbsp;&nbsp;`Application.java`                   | Server entrypoint                                     |
| &nbsp;&nbsp;&nbsp;&nbsp;`ApplicationReadyManager.java`       | Manager class used to register all internal listeners |
| &nbsp;&nbsp;&nbsp;&nbsp;`EventBroadcaster.java`              | Class responsible for generating async events         |

### How to deploy this application on your Ubuntu server?

First of all, make sure you have **OpenJDK 21** installed on your server.
It's recommended to use OpenJDK distribution for your JDK.

```bash
sudo apt install openjdk-21-jdk
```

If you have an older version, you can easily uninstall it:

```bash
sudo apt-get purge openjdk*
```

Once the application config is ready, you can prepare a script to launch the app:

```bash
#!/bin/bash

java -Xmx1G -XX:+UseG1GC -jar /home/path/myworkflows.org/public_html/myworkflows-app.jar --spring.config.location=/home/path/myworkflows.org/public_html/application.yml
```
