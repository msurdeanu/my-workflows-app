# MyWorkflows

[MyWorkflows](https://myworkflows.org) is a simple tool for creating workflows to automate daily manual tasks.
You can easily create workflows, schedule them using cron-jobs and see their output.

Uses Java as programming language and Vaadin as UI framework.

This tool is perfect for small teams which are flooded by manual tasks day by day.
All workflows are defined in JSON format and for playing with them, you have a nice UI.

## Technology stack

* Java 17 as programming language.
* Spring Boot 3.x as dependency injection framework
* [Vaadin 24](https://vaadin.com) as UI framework
* [SQLite](https://www.sqlite.org/) as relational database for persisting data.
* Use [Groovy](https://groovy-lang.org/) as additional language for defining workflow commands
* Use [SpEL](https://docs.spring.io/spring-framework/docs/3.0.x/reference/expressions.html) as runtime evaluator to propagate data between two or multiple commands.

## Features

* **Modern** and **responsive** UI.
* **Authentication** and **authorization** enabled by default.
* Vaadin **Push** enabled. Through websockets, the server can send updates to the client.
* Persistence layer based on **SQLite**.
* Dedicated web pages for **workflow development**, **workflow templates** and **workflow runs**.
* Dedicated web page for **statistics**.

## TODOs

- [ ] **Checkpoints**. Introduce ability to reschedule a failing workflow from a specific moment in time.
- [ ] **Debug mode**. Improve debugging experience for failing workflows to easily find the root cause of their failure.

## How it works?

### Abbreviations

* **Command**: Represents a standalone entity for executing an operation. A command has a name, a type, inputs, asserts and outputs.
* **Workflow**: Represents one or multiple commands which will be executed on a single thread and in a sequential order.
* **Input**: Each commands needs some inputs in order to do the expected operation.
* **Assert**: Once a command is completed, the output, if exists, can be validated through a set of assertions.
* **Output**: Once a command is completed, the output, if exists, can be processed further more and saved in other variables.
* **Variable**: Represents a standalone data variable which is generated by a command and can be used as input by upcoming commands.

### Workflow definition

`Workflow` is the logic entity that encapsulates multiple steps - called `commands` - need to implement a task.
Each workflow has a `name` to be able to identify easily what is doing.

When the time comes, the workflow can be run `manually` by the user or `automatically` using a scheduler.
Behind the scene, there is a `thread pool` responsible for executing the workflow.
The thread is configured to run commands in the same order as they are defined in workflow definition.

### Placeholders

You are allowed to use global `placeholders` to avoid data duplication in your workflow definition.
Please note that all placeholders are resolved immediately before workflow running process.
All placeholders are persisted in a database - a table called `placeholders`.
Placeholders are mandatory, as a result, if one of them is not found, you will get en exception and your workflow will not continue his execution.

Inside workflow definition, you can recognize a placeholder by having this format: `$(NAME)`.
You are allowed to use placeholders inside any `input`, `assert` and `output` (`name` and `value` fields).

### Comments

From technical point of view, each workflow is defined in JSON format.
If you are familiar with JSON, you probably know that comments are not allowed.
Since comments are useful sometimes, there is a hack that can be implemented to have this wonderful feature: by using a dedicated field called `_comment`.
This field can be used anywhere in the workflow definition.

### Finally commands

The concept of `finally commands` lets you declare some special commands for your workflow to be executed at the end, no matter if the workflow failed.
The concept is similar with `try-finally` block functionality from Java.

```
try {
    // Run all commands in a sequential order
} finally {
    // Run all finallyCommands in a sequential order
}
```

## Predefined type of commands

### Groovy command

Provides ability to run Groovy code at runtime.
As you probably already imagine, this command is very powerful.

| `@type`  | Inputs                                                                                                                                                                                                                                                           | Output                             |
|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------|
| `groovy` | <ul><li><strong>scriptLines</strong>: Mandatory. Represents source code which contains definition of a `methodName` (or `run`) method to be executed.</li><li><em>methodName</em>: Optional. Represents the method name invoked when code is executed.</li></ul> | Return of invoked method or `void` |

Example of a dummy command:
```json
{
  "name":"Run empty method",
  "@type":"groovy",
  "inputs":[
    {
      "name":"scriptLines",
      "value":[
        "def run(executionCache) {",
        "}"
      ]
    },
    {
      "name":"methodName",
      "value":"run"
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
  "name":"Inject input parameters or process existing ones",
  "@type":"nothing",
  "inputs":[
    {
      "name":"test",
      "value":"Just a simple test"
    }
  ]
}
```

### Print command

Captures an input / output variable during workflow execution and shows the value to the UI.

| `@type` | Inputs                                                                                                                                                                                                           | Output                                                                |
|---------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------|
| `print` | <ul><li><strong>keys</strong>: Mandatory. Represents a list of variable names that will be displayed.</li></ul> | Returns total number of keys affected by this operation. Type: `int`. |

Example of a dummy command:
```json
{
  "name":"Print 'commandOutput' value",
  "@type":"print",
  "inputs":[
    {
      "name":"keys",
      "value":[
        "commandOutput"
      ]
    }
  ]
}
```

### Sleep command

Provides ability to pause current workflow execution by a given time.
Time unit is milliseconds.

| `@type` | Inputs                                                                                              | Output                                       |
|---------|-----------------------------------------------------------------------------------------------------|----------------------------------------------|
| `sleep` | <ul><li><strong>sleepTime</strong>: Mandatory. Represents number of millis used to sleep.</li></ul> | Returns the actual time slept. Type: `long`. |

Example of a dummy command:
```json
{
  "name":"Sleep for one second",
  "@type":"sleep",
  "inputs":[
    {
      "name":"sleepTime",
      "value":1000
    }
  ]
}
```

## Example of a dummy workflow

```json
{
  "name":"Test",
  "commands":[
    {
      "name":"Just a sleep",
      "@type":"sleep",
      "inputs":[
        {
          "name":"sleepTime",
          "value":5000
        }
      ],
      "outputs":[
        {
          "name":"$(TEST)",
          "value":5000
        }
      ]
    },
    {
      "name":"Just a print",
      "@type":"print",
      "inputs":[
        {
          "name":"keys",
          "value":[
            "$(TEST)"
          ]
        }
      ]
    }
  ]
}
```

## Running the application
There are two ways to run the application: using `mvn spring-boot:run` or by running the `Application` class directly from your IDE.

You can use any IDE of your preference, but I suggest IntelliJ IDEA.

## Structure

Vaadin web applications are full-stack and include both client-side and server-side code in the same project.

| Directory                                                 | Description                                           |
|:----------------------------------------------------------|:------------------------------------------------------|
| `frontend/`                                               | Client-side source directory                          |
| &nbsp;&nbsp;&nbsp;&nbsp;`themes/`                         | Themes directory (CSS)                                |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`simple/` | Default theme                                         |
| `src/main/java/org/myworkflows`                           | Server-side source directory                          |
| &nbsp;&nbsp;&nbsp;&nbsp;`component/`                      | Package with all Vaadin custom components             |
| &nbsp;&nbsp;&nbsp;&nbsp;`config/`                         | Package with different Spring configuration beans     |
| &nbsp;&nbsp;&nbsp;&nbsp;`domain/`                         | Package with all classes part of application domain   |
| &nbsp;&nbsp;&nbsp;&nbsp;`provider/`                       | Java package with a set of providers                  |
| &nbsp;&nbsp;&nbsp;&nbsp;`repository/`                     | Contains a set of JpaRepositories                     |
| &nbsp;&nbsp;&nbsp;&nbsp;`service/`                        | Contains a set of services exposed by the application |
| &nbsp;&nbsp;&nbsp;&nbsp;`view/`                           | Contains a set of views exposed by the application    |
| &nbsp;&nbsp;&nbsp;&nbsp;`Application.java`                | Server entrypoint                                     |

