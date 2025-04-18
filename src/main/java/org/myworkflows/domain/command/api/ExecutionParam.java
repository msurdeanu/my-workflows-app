package org.myworkflows.domain.command.api;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.PARAMETER)
public @interface ExecutionParam {

    boolean required() default true;

    boolean bypassed() default false;

    String defaultValue() default "";

}
