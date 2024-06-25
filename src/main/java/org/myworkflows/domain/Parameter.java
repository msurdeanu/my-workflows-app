package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import org.myworkflows.converter.ParameterTypeToStringConverter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Getter
@Table(name = "parameters")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Parameter {

    @Id
    @EqualsAndHashCode.Include
    private String name;

    @Convert(converter = ParameterTypeToStringConverter.class)
    private ParameterType type;

    @Column(name = "default_value")
    private String defaultValue;

    public Object getComputedDefaultValue() {
        return type.getComputedDefaultValue(defaultValue);
    }

}
