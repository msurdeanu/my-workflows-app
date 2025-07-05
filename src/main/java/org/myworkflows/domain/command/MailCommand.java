package org.myworkflows.domain.command;

import jakarta.mail.Authenticator;
import jakarta.mail.Message;
import jakarta.mail.MessagingException;
import jakarta.mail.PasswordAuthentication;
import jakarta.mail.Session;
import jakarta.mail.Transport;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeBodyPart;
import jakarta.mail.internet.MimeMessage;
import jakarta.mail.internet.MimeMultipart;
import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;

import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static com.networknt.schema.utils.StringUtils.isBlank;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class MailCommand extends AbstractCommand {

    public static final String PREFIX = "mail";

    public MailCommand(String name,
                       Set<ExpressionNameValue> ifs,
                       Set<ExpressionNameValue> inputs,
                       Set<ExpressionNameValue> asserts,
                       Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = PREFIX)
    public void mail(@ExecutionParam String from,
                     @ExecutionParam String to,
                     @ExecutionParam String subject,
                     @ExecutionParam String body,
                     @ExecutionParam Map<String, Object> props,
                     @ExecutionParam(required = false, defaultValue = "text/html; charset=utf-8") String bodyType,
                     @ExecutionParam(required = false) String cc,
                     @ExecutionParam(required = false) String bcc,
                     @ExecutionParam(required = false) String username,
                     @ExecutionParam(required = false) String password) {
        final var properties = new Properties();
        properties.putAll(props);
        final var session = Session.getInstance(properties, isBlank(username) ? null : new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(username, password);
            }
        });

        WorkflowRuntimeException.wrap(() -> {
            final var message = new MimeMessage(session);
            message.setFrom(new InternetAddress(from));
            message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(to));
            setMessageRecipientsIfNotNull(message, Message.RecipientType.CC, cc);
            setMessageRecipientsIfNotNull(message, Message.RecipientType.BCC, bcc);
            message.setSubject(subject);

            final var mimeBodyPart = new MimeBodyPart();
            mimeBodyPart.setContent(body, bodyType);

            final var multipart = new MimeMultipart();
            multipart.addBodyPart(mimeBodyPart);
            message.setContent(multipart);

            Transport.send(message);
            return null;
        });
    }

    private void setMessageRecipientsIfNotNull(MimeMessage message, Message.RecipientType recipientType, String recipient) throws MessagingException {
        if (recipient != null) {
            message.setRecipients(recipientType, InternetAddress.parse(recipient));
        }
    }

}
