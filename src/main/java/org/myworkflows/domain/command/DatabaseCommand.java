package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.MandatoryParam;

import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Optional;
import java.util.Set;

import static java.util.Optional.empty;
import static java.util.Optional.of;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class DatabaseCommand extends AbstractCommand {

    public DatabaseCommand(String name,
                           Set<ExpressionNameValue> ifs,
                           Set<ExpressionNameValue> inputs,
                           Set<ExpressionNameValue> asserts,
                           Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod
    public Optional<ResultSet> database(@MandatoryParam String url,
                                        @MandatoryParam String query) throws SQLException {
        try (final var connection = DriverManager.getConnection(url); // format: "jdbc:sqlite:"
             final var statement = connection.createStatement()) {
            final var resultSet = statement.executeQuery(query); // TODO: escape sql query
            if (resultSet != null && resultSet.isBeforeFirst()) {
                return of(resultSet);
            } else {
                return empty();
            }
        }
    }

}
