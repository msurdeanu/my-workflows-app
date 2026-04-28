package org.myworkflows;

import com.vaadin.flow.component.dependency.StyleSheet;
import com.vaadin.flow.component.page.AppShellConfigurator;
import com.vaadin.flow.component.page.Push;
import com.vaadin.flow.theme.lumo.Lumo;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@SpringBootApplication
@Push
@StyleSheet(Lumo.STYLESHEET)
@StyleSheet("styles.css")
public class Application implements AppShellConfigurator {

    static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

}
