package org.myworkflows.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Mihai Surdeanu
 * @since 1.0
 */
@Entity
@Getter
@Setter
@Table(name = "doc_pages")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class DocPage {

    @Id
    @EqualsAndHashCode.Include
    private String name;

    @Column(name = "value")
    private String value;

    public static DocPage of(String name) {
        final var docPage = new DocPage();
        docPage.setName(name);
        docPage.setValue(StringUtils.EMPTY);
        return docPage;
    }

}
