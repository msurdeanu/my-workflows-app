package org.myworkflows.domain.command;

import lombok.NoArgsConstructor;
import org.myworkflows.domain.ExpressionNameValue;
import org.myworkflows.domain.command.api.ExecutionMethod;
import org.myworkflows.domain.command.api.ExecutionParam;
import org.myworkflows.exception.WorkflowRuntimeException;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.Set;

import static java.time.Duration.ofMillis;
import static org.springframework.http.HttpMethod.valueOf;
import static org.springframework.web.util.UriComponentsBuilder.fromUriString;

/**
 * @author Mihai Surdeanu
 * @since 1.0.0
 */
@NoArgsConstructor
public final class HttpRequestCommand extends AbstractCommand {

    public static final String PREFIX = "httpRequest";

    public HttpRequestCommand(String name,
                              Set<ExpressionNameValue> ifs,
                              Set<ExpressionNameValue> inputs,
                              Set<ExpressionNameValue> asserts,
                              Set<ExpressionNameValue> outputs) {
        super(name, ifs, inputs, asserts, outputs);
    }

    @ExecutionMethod(prefix = "httpRequest")
    public ResponseEntity<String> httpRequest(@ExecutionParam String url,
                                              @ExecutionParam(required = false, defaultValue = "GET") String method,
                                              @ExecutionParam(required = false) String body,
                                              @ExecutionParam(required = false, defaultValue = ":of") Map<String, String> headers,
                                              @ExecutionParam(required = false, defaultValue = "60000") Long timeout,
                                              @ExecutionParam(required = false, defaultValue = "false") Boolean skipSsl) {
        return createRestTemplate(timeout, skipSsl).exchange(fromUriString(url).build(true).toUri(), valueOf(method),
            createHttpEntity(body, headers), String.class);
    }

    private RestTemplate createRestTemplate(long timeout, boolean skipSsl) {
        if (skipSsl) {
            return createRestTemplateWithoutSslValidation(timeout);
        } else {
            return new RestTemplateBuilder()
                .setConnectTimeout(ofMillis(timeout))
                .setReadTimeout(ofMillis(timeout))
                .build();
        }
    }

    private HttpEntity<String> createHttpEntity(String body, Map<String, String> headers) {
        final var httpHeaders = new HttpHeaders();
        headers.forEach(httpHeaders::set);
        return new HttpEntity<>(body, httpHeaders);
    }

    private RestTemplate createRestTemplateWithoutSslValidation(long timeout) {
        try {
            final var trustAllCerts = new TrustManager[] {
                new X509TrustManager() {
                    public void checkClientTrusted(X509Certificate[] certs, String authType) {
                    }

                    public void checkServerTrusted(X509Certificate[] certs, String authType) {
                    }

                    public X509Certificate[] getAcceptedIssuers() {
                        return null;
                    }
                }
            };

            final var sslContext = SSLContext.getInstance("TLS");
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
            HostnameVerifier allHostsValid = (hostname, session) -> true;
            HttpsURLConnection.setDefaultSSLSocketFactory(sslContext.getSocketFactory());
            HttpsURLConnection.setDefaultHostnameVerifier(allHostsValid);

            final var requestFactory = new SimpleClientHttpRequestFactory();
            requestFactory.setConnectTimeout(ofMillis(timeout));
            requestFactory.setReadTimeout(ofMillis(timeout));
            return new RestTemplate(requestFactory);
        } catch (Exception e) {
            throw new WorkflowRuntimeException(e);
        }
    }

}
