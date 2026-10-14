# From development perspective

## Running the application

There are two ways to run the application: using `mvn spring-boot:run` or by running the `Application` class directly
from your IDE.

You can use any IDE of your preference, but I suggest IntelliJ IDEA.

## Application bootstrap phase

During the application bootstrap phase, multiple local caches are filled with information found in a database.
The data is loaded in the following order:

1. Once the **application is ready**, the app will load any kind of **external JAR** provided by the user.
2. After all **external JARs** are loaded, the app will proceed with loading all **workflow placeholders** from
   a database.
3. Once all **placeholders** are loaded, the app will continue with **workflow parameters**.
4. Once all **parameters** are loaded, the app will continue with **workflow definitions**.
5. When all **definitions** are available, the app will load all **workflow templates** found in a database. They
   are also scheduled.
6. The next step is to load all **workflow runs**. In fact, is not all of them. The app will load only the
   accepted capacity for workflow runs defined in application config.
7. Last but not least, the app will load all documentation pages available in the local database.

## Structure

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
| &nbsp;&nbsp;&nbsp;&nbsp;`migration/`                         | Database migrations which SQL alone cannot express    |
| &nbsp;&nbsp;&nbsp;&nbsp;`provider/`                          | Java package with a set of providers                  |
| &nbsp;&nbsp;&nbsp;&nbsp;`repository/`                        | Contains a set of JpaRepositories                     |
| &nbsp;&nbsp;&nbsp;&nbsp;`restapi/`                           | Exposes REST API part for this app                    |
| &nbsp;&nbsp;&nbsp;&nbsp;`service/`                           | Contains a set of services exposed by the application |
| &nbsp;&nbsp;&nbsp;&nbsp;`view/`                              | Contains a set of views exposed by the application    |
| &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`component/` | Package with all Vaadin custom components             |
| &nbsp;&nbsp;&nbsp;&nbsp;`Application.java`                   | Server entrypoint                                     |
| &nbsp;&nbsp;&nbsp;&nbsp;`ApplicationReadyManager.java`       | Manager class used to register all internal listeners |
| &nbsp;&nbsp;&nbsp;&nbsp;`EventBroadcaster.java`              | Class responsible for generating async events         |

## How to deploy this application to your Ubuntu server?

First, make sure you have **OpenJDK 21** installed on your server.
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
