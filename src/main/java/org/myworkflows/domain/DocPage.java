package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Entity
@Getter
@Setter
@Table(name = "doc_pages")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class DocPage implements CacheableEntry {

    @Id
    @EqualsAndHashCode.Include
    private String name;

    @Column(name = "value")
    private String value;

    @Override
    public Object getCacheableKey() {
        return name;
    }

}
