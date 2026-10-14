# Samples of workflow definitions

This page contains a set of workflow definition samples that you can use to build your next workflow pipeline. :)

## Simple workflow for performing an HTTP request

This workflow will perform a GET request to URL `https://jsonplaceholder.typicode.com/todos/1`, will assert that response status code is 200 OK and will save the output returned by the request into a variable named `requestBody`. This variable will be printed and visible for end users.

```yaml
# Simple HTTP request workflow
commands:
- name: Make HTTP request
  class: httpRequest
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/todos/1
  asserts:
  - name: Check status code
    class: java
    value: output.getStatusCode() == org.springframework.http.HttpStatus.OK
  outputs:
  - name: requestBody
    class: java
    value: output.getBody()
- name: Print 'requestBody' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - requestBody
```

Details about implementation:
* 2 commands are used: `httpRequest` + `print`.
* we are playing with `inputs`, `asserts` and `outputs`.
* as runtime evaluator, we are using Java compiler.

## Simple workflow for integrating an external library

This workflow will integrate [JsonPath](https://github.com/json-path/JsonPath) library to extend Java functionality.
The library is loaded as an external JAR when application is starting, during bootstrap phase.

For being able to run this workflow, please upload the following external JARs to the `libs` folder and set application properties:
```yaml
my-workflows:
  config:
    library:
      base-directory: "./libs"
# accessors-smart-2.5.0.jar
# json-smart-2.5.0.jar
# json-path-2.9.0.jar
```

Once application config is changed and application is restarted, please run the following workflow:
```yaml
commands:
- name: Parse JSON using JsonPath library
  class: java
  inputs:
  - name: java.script
    value: |
      import com.jayway.jsonpath.JsonPath;
      import org.myworkflows.domain.WorkflowRunCache;
      public class DynamicClass {
        public Object run(WorkflowRunCache cache) {
          return JsonPath.parse("[{'a':1,'b':2}]").read("$[0]['b']");
        }
      }
  outputs:
  - name: test
    class: java
    value: output
- name: Print 'test' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - test
```

Details about implementation:
* 2 commands are used: `java` + `print`.
* we are playing with `inputs` and `outputs`.
* as runtime evaluator, we are still using Java compiler.

### How to process outputs using JsonPath library?

Using `java` as runtime evaluator:
```yaml
commands:
- name: Make HTTP request
  class: httpRequest
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/todos/1
  asserts:
  - name: Check status code
    class: java
    value: output.getStatusCode() == org.springframework.http.HttpStatus.OK
  outputs:
  - name: title
    class: java
    value: "com.jayway.jsonpath.JsonPath.parse((String) output.getBody()).read(\"$.title\")"
- name: Print 'title' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - title
```

Using `spel` as runtime evaluator:
```yaml
commands:
- name: Make HTTP request
  class: httpRequest
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/todos/1
  asserts:
  - name: Check status code
    class: spel
    value: "#output.getStatusCode() == T(org.springframework.http.HttpStatus).OK"
  outputs:
  - name: title
    class: spel
    value: "T(com.jayway.jsonpath.JsonPath).parse(#output.getBody()).read('$.title')"
- name: Print 'title' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - title
```

Using `groovy` as runtime evaluator:
```yaml
commands:
- name: Make HTTP request
  class: httpRequest
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/todos/1
  asserts:
  - name: Check status code
    class: groovy
    value: "output.getStatusCode() == org.springframework.http.HttpStatus.OK"
  outputs:
  - name: title
    class: groovy
    value: "com.jayway.jsonpath.JsonPath.parse(output.getBody()).read('$.title')"
- name: Print 'title' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - title
```

## Simple loop command with sleep inside

This workflow defines a list of 3 integers = `List.of(1000, 2000, 3000)` and submits this list to a `loop` command which will iterate over each element and will call a `sleep` command which will sleep exactly the time provided (in milliseconds). Workflow total run time will be around 6 seconds.

The workflows show an interesting approach when output is present inside the subcommand, since the type of this variable is a `Map`.

```yaml
commands:
- name: Loop over 3 items and sleep accordingly for each of them
  class: loop
  inputs:
  - name: loop.items
    value:
    - 1000
    - 2000
    - 3000
  - name: loop.backoffPeriod
    value: 1000
  subcommands:
  - name: Sleep a while
    class: sleep
    inputs:
    - name: sleep.time
      class: groovy
      value: $(loop.item)
    outputs:
    - name: executionTime
      class: groovy
      value: output
- name: Print 'executionTime' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - executionTime
```

Details about implementation:
* 3 different commands are used: `loop` (with subcommand `sleep`) + `print`.
* we are playing with cache access pattern feature to access current loop item from workflow local cache: `$(loop.item)`
* Groovy is used as runtime evaluator.

## Simple workflow with `finally` command

```yaml
commands:
- name: Sleep without any time
  class: sleep
  #inputs:
  #- name: sleep.time
  #  value: 0
finallyCommands:
- name: This command executes even if there is an exception reported
  class: sleep
  inputs:
  - name: sleep.time
    value: 3000
```

Details about implementation:
* finally commands are run even if there is an exception.
* finally commands will not affect the status of the entire workflow. In our case, the workflow will fail because `sleep.time` is undefined for the first command, but it will fail after 3 seconds.

## Simple workflow with `ifs` condition

```yaml
commands:
- name: Generate random number
  class: nothing # Java command can be also used to generate the random number
  inputs:
  - name: random.number
    class: groovy
    value: new java.util.Random().nextInt(100)
- name: Print 'random.number' variable only if we deal with an even number
  class: print
  ifs:
  - name: Check if even number # if the number is odd the command will be skipped
    class: groovy
    value: $(random.number) % 2 == 0
  inputs:
  - name: print.keys
    value:
    - random.number
```

Details about implementation:
* 2 different commands are used: `nothing` + `print`.
* 1st command is responsible for generating a random number between 0 and 99 (included).
* 2nd command will run only if the number generated at step 1 is even. This second command is responsible for printing the number.
* Groovy is used as runtime evaluator.

## Workflow which waits until a service is available

This workflow calls `https://jsonplaceholder.typicode.com/todos/1` up to 5 times, with a pause of 2 seconds between two attempts, and stops calling as soon as the service answers with 200 OK. The number of attempts which were not needed is saved into a variable named `unusedRounds`.

```yaml
commands:
- name: Wait until the service answers
  class: waitUntilSubPasses
  inputs:
  - name: waitUntilSubPasses.rounds
    value: 5
  - name: waitUntilSubPasses.backoffPeriod
    value: 2000
  subcommands:
  - name: Call the service
    class: httpRequest
    inputs:
    - name: httpRequest.url
      value: https://jsonplaceholder.typicode.com/todos/1
    asserts:
    - name: Check status code
      class: spel
      value: "#output.getStatusCode().value() == 200"
  asserts:
  - name: The service answered in time
    class: spel
    value: "#output >= 0"
  outputs:
  - name: unusedRounds
    class: spel
    value: "#output"
- name: Print 'unusedRounds' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - unusedRounds
```

Details about implementation:
* 3 different commands are used: `waitUntilSubPasses` (with subcommand `httpRequest`) + `print`.
* a round fails when a subcommand throws an exception or when one of its asserts fails. An answer with a 4xx or a 5xx status code makes `httpRequest` throw an exception, so it counts as a failed round too.
* `waitUntilSubPasses` itself never fails: its output is `-1` when every round failed. This is why the assert `#output >= 0` is there - without it, the workflow would go on as if the service was available.
* SpEL is used as runtime evaluator.

## Workflow which parses a JSON response without any external library

[Jackson](https://github.com/FasterXML/jackson) is part of the application, so a JSON response can be parsed without uploading any JAR. This workflow reads a to-do item, converts the response body into a `Map` and saves two of its fields into the variables `todo.title` and `todo.completed`.

```yaml
commands:
- name: Read a to-do item
  class: httpRequest
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/todos/1
  outputs:
  - name: todo
    class: groovy
    value: new com.fasterxml.jackson.databind.ObjectMapper().readValue(output.getBody(), Map.class)
  - name: todo.title
    class: groovy
    value: $(todo).title
  - name: todo.completed
    class: groovy
    value: $(todo).completed
- name: Print what was read
  class: print
  inputs:
  - name: print.keys
    value:
    - todo.title
    - todo.completed
```

Details about implementation:
* 2 commands are used: `httpRequest` + `print`.
* outputs are processed in the order they are written and each of them is saved right away. This is why the 2nd and the 3rd output can already read the variable `todo` saved by the 1st one, so the body is parsed only once.
* a cache access pattern can be followed by more code: `$(todo).title` reads the variable `todo` and then the key `title` of that map.
* Groovy is used as runtime evaluator.

## Workflow which loops over the result of a previous command

This workflow reads the first 3 users, then asks for the open to-do items of each of them. The items of the `loop` command are not written in the definition: they are the ids returned by the first request.

```yaml
commands:
- name: Read the first 3 users
  class: httpRequest
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/users?_limit=3
  outputs:
  - name: userIds
    class: groovy
    value: new com.fasterxml.jackson.databind.ObjectMapper().readValue(output.getBody(), List.class).collect { it.id }
- name: Count the open to-do items of every user
  class: loop
  inputs:
  - name: loop.items
    class: groovy
    value: $(userIds)
  - name: loop.backoffPeriod
    value: 0
  subcommands:
  - name: Read the open to-do items of the current user
    class: httpRequest
    inputs:
    - name: httpRequest.url
      class: groovy
      value: "'https://jsonplaceholder.typicode.com/todos?completed=false&userId=' + $(loop.item)"
    outputs:
    - name: openTodos
      class: groovy
      value: new com.fasterxml.jackson.databind.ObjectMapper().readValue(output.getBody(), List.class).size()
- name: Build a summary
  class: groovy
  inputs:
  - name: groovy.script
    value: |
      def run(cache) {
        cache.get('openTodos').sort().collect { userId, count -> 'user ' + userId + ' has ' + count }.join(', ')
      }
  outputs:
  - name: summary
    class: groovy
    value: output
- name: Print 'openTodos' and 'summary' variables
  class: print
  inputs:
  - name: print.keys
    value:
    - openTodos
    - summary
```

Details about implementation:
* 4 different commands are used: `httpRequest`, `loop` (with subcommand `httpRequest`), `groovy` + `print`.
* `loop.items` is an expression which returns the list saved by the first command. The list has to contain at least one item, otherwise the `loop` command fails.
* `openTodos` is an output of a subcommand, so it is a `Map`: its keys are the loop items (the user ids) and its values are the results of the iterations.
* the `groovy` command reads that map from the cache and returns one line of text, which becomes the variable `summary`.
* `loop.backoffPeriod` is set to `0`, as there is no reason to wait for one second between two users.

## Workflow which queries a database

This workflow counts the workflow runs saved in the database of the application itself and how many of them failed. No setup is needed, as the JDBC driver of SQLite is part of the application.

```yaml
commands:
- name: Count the workflow runs
  class: database
  inputs:
  - name: database.url
    value: "jdbc:sqlite:database.db"
  - name: database.query
    value: "SELECT COUNT(*) AS total, COUNT(failure_message) AS failed FROM workflow_runs"
  asserts:
  - name: Query returned a row
    class: groovy
    value: output.isPresent()
  outputs:
  - name: runs
    class: groovy
    value: |
      def row = output.get()
      row.next()
      return [total: row.getInt('total'), failed: row.getInt('failed')]
- name: Print 'runs' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - runs
```

Details about implementation:
* 2 commands are used: `database` + `print`.
* the output of the `database` command is an `Optional<ResultSet>`, which is empty when the query returns no row. This is why the assert checks `output.isPresent()` before the output reads `output.get()`.
* the result set starts before its first row, so `next()` has to be called before a column is read.
* an expression can have more than one line. Here it returns a map, so the workflow prints something like `{total=42, failed=3}`.
* `database.db` is searched in the working directory of the application. Use an absolute path if the workflow has to read another SQLite file.

## Workflow with a manual approval

This workflow stops before its last step and waits until a user approves it. While it waits, the run is listed as *waiting for approval* on the runs page, where it can be approved or rejected. A run started from the development page can be answered there as well.

```yaml
commands:
- name: Prepare the release
  class: nothing
  inputs:
  - name: release.version
    value: 2.1.0
- name: Ask for an approval
  class: approval
  inputs:
  - name: approval.message
    class: groovy
    value: "'Release ' + $(release.version) + ' is ready to go live.'"
  - name: approval.timeout
    value: 600000
  outputs:
  - name: approvedBy
    class: groovy
    value: output
- name: Go live
  class: sleep
  inputs:
  - name: sleep.time
    value: 2000
finallyCommands:
- name: Print the release and who approved it
  class: print
  inputs:
  - name: print.keys
    value:
    - release.version
    - approvedBy
```

Details about implementation:
* 4 different commands are used: `nothing`, `approval`, `sleep` + `print`. The `sleep` command only stands for the real work, like a `sshExec` or a `httpRequest` command.
* the message shown to the approver is an expression, so it can contain what the previous commands found out.
* the run waits for 10 minutes at most. When nobody answers in time or when the run is rejected, the `approval` command fails: `Go live` is skipped and the workflow is marked as failed.
* the output of the `approval` command is the name of the user who approved.
* the `print` command is a `finally` command, so the release is printed after a rejection too. In that case the variable `approvedBy` does not exist and is simply left out.

## Workflow which sends a notification only when it fails

`Finally` commands always run, so a notification placed there would be sent after every run. This workflow sends it only after a failure: its last command saves a variable, and the `finally` command runs only if that variable is missing.

The first command fails on purpose, because `https://jsonplaceholder.typicode.com/todos/0` answers with 404 Not Found.

```yaml
commands:
- name: Call a page which does not exist
  class: httpRequest
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/todos/0
- name: Remember that every command passed
  class: nothing
  inputs:
  - name: workflow.passed
    value: true
finallyCommands:
- name: Tell the team that the workflow failed
  class: httpRequest
  ifs:
  - name: Not every command passed
    class: groovy
    value: $(workflow.passed) == null
  inputs:
  - name: httpRequest.url
    value: https://jsonplaceholder.typicode.com/posts
  - name: httpRequest.method
    value: POST
  - name: httpRequest.headers
    value:
      Content-Type: application/json
  - name: httpRequest.body
    value: '{"title": "Nightly check failed", "body": "Have a look at the workflow runs page."}'
  asserts:
  - name: Notification was accepted
    class: spel
    value: "#output.getStatusCode().is2xxSuccessful()"
```

Details about implementation:
* 2 different commands are used: `httpRequest` + `nothing`.
* the command which saves `workflow.passed` has to be the last one of `commands`: it is reached only if all the commands before it passed.
* a cache access pattern without a type, like `$(workflow.passed)`, gives `null` when the variable does not exist, instead of raising an exception.
* replace the URL and the body of the `finally` command with the ones of the incoming webhook of your chat tool. `https://jsonplaceholder.typicode.com/posts` only pretends to save what it receives.
* inputs are variables too and they are kept after their command is over. A `httpRequest` command placed after this one would also send a `POST` with the same headers and body, unless it sets these inputs again.

## Workflow driven by a parameter

This workflow reads the to-do item whose id is given by a workflow parameter named `todoId`. To try it, add a parameter with this name, type `Integer` and a value between 1 and 200 under *Workflow Parameters*, on the development page. Without the parameter, the workflow reads the first item.

```yaml
commands:
- name: Use the first to-do item when no parameter is given
  class: nothing
  ifs:
  - name: Parameter 'todoId' is missing
    class: groovy
    value: $(todoId) == null
  inputs:
  - name: todoId
    value: 1
- name: Read the to-do item
  class: httpRequest
  inputs:
  - name: httpRequest.url
    class: groovy
    value: "'https://jsonplaceholder.typicode.com/todos/' + $(todoId:Integer.class)"
  outputs:
  - name: todoTitle
    class: groovy
    value: new com.fasterxml.jackson.databind.ObjectMapper().readTree(output.getBody()).get('title').asText()
- name: Print 'todoId' and 'todoTitle' variables
  class: print
  inputs:
  - name: print.keys
    value:
    - todoId
    - todoTitle
```

Details about implementation:
* 3 different commands are used: `nothing`, `httpRequest` + `print`.
* a parameter is a variable which is already there when the first command starts. A command with an `if` on a missing variable is an easy way to give a parameter a default value.
* `$(todoId:Integer.class)` also checks the type: the workflow fails if the parameter was added with another type, like `String`.
* when the workflow is run by a template, select the parameter in that template. A parameter named `todoId.42` is injected as `todoId`, but only for the template with id 42.
* add a parameter named `debug`, of type `Boolean` and set to `true`, to see the type and the value of every input of every command.

## Workflow using placeholders, SSH and mail

This workflow connects to a server through SSH, reads how full its root partition is and sends a mail if the usage is 80% or more. Nothing about the server or the mail account is written in the definition: everything comes from placeholders.

For being able to run this workflow, please add the following placeholders on the *Workflow Placeholders* page: `SSH_HOST`, `SSH_USERNAME`, `SSH_PASSWORD`, `SMTP_HOST`, `MAIL_FROM` and `MAIL_TO`.

```yaml
commands:
- name: Read the usage of the root partition
  class: sshExec
  inputs:
  - name: sshExec.host
    value: $$(SSH_HOST)
  - name: sshExec.username
    value: $$(SSH_USERNAME)
  - name: sshExec.password
    value: $$(SSH_PASSWORD)
  - name: sshExec.command
    value: df --output=pcent / | tail -n 1
  asserts:
  - name: Command is successful
    class: spel
    value: "#output.getExitCode() == 0"
  outputs:
  - name: diskUsage
    class: groovy
    value: output.getOutput().replaceAll('[^0-9]', '').toInteger()
- name: Send a mail when the partition is almost full
  class: mail
  ifs:
  - name: Usage is 80% or more
    class: groovy
    value: $(diskUsage) >= 80
  inputs:
  - name: mail.from
    value: $$(MAIL_FROM)
  - name: mail.to
    value: $$(MAIL_TO)
  - name: mail.subject
    class: groovy
    value: "'Disk of $$(SSH_HOST) is ' + $(diskUsage) + '% full'"
  - name: mail.body
    class: groovy
    value: "'The root partition of <strong>$$(SSH_HOST)</strong> is ' + $(diskUsage) + '% full.'"
  - name: mail.props
    value:
      mail.smtp.host: $$(SMTP_HOST)
      mail.smtp.port: 25
- name: Print 'diskUsage' variable
  class: print
  inputs:
  - name: print.keys
    value:
    - diskUsage
```

Details about implementation:
* 3 different commands are used: `sshExec`, `mail` + `print`.
* placeholders are replaced right before a command runs, as plain text. This is why a placeholder can also be used in the middle of an expression, like in `mail.subject`.
* the output of `sshExec` has an exit code and the text printed by the command, something like ` 42%`. The output expression keeps only the digits and converts them to an `Integer`.
* the `mail` command has an `if`, so it is skipped when the usage is below 80%. The workflow still passes in that case.
* the SSH password ends up in a variable named `sshExec.password`. Every variable whose name contains `password` is masked when it is printed, so it is not visible in debug mode either.
* this SMTP server asks for no authentication. For one which does, add `mail.username` and `mail.password` as inputs, plus `mail.smtp.auth` and `mail.smtp.starttls.enable` set to `true` in `mail.props`.