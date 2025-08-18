package org.myworkflows.domain;

import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.Getter;
import lombok.Setter;
import org.myworkflows.converter.SettingTypeToStringConverter;

/**
 * @author Mihai Surdeanu
 * @since 1.1
 */
@Entity
@Getter
@Table(name = "settings")
public class Setting {

    @Id
    private String key;

    private String title;

    private String description;

    @Convert(converter = SettingTypeToStringConverter.class)
    private SettingType type;

    @Setter
    private String value;

    private boolean editable;

    private int position;

    @Transient
    @Setter
    private Object computedValue;

}
