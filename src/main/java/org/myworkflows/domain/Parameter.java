package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.converter.ParameterTypeToStringConverter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Getter
@Setter
@Table(name = "parameters")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Parameter {

    @Id
    @EqualsAndHashCode.Include
    private String name;

    @Convert(converter = ParameterTypeToStringConverter.class)
    private ParameterType type;

    @Column(name = "value")
    private String value;

    public Object getComputedValue() {
        return type.getComputedValue(value);
    }

}
