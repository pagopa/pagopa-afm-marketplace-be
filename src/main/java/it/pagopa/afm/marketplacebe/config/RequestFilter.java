package it.pagopa.afm.marketplacebe.config;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.UUID;

import static it.pagopa.afm.marketplacebe.util.Constants.HEADER_REQUEST_ID;

@Component
//@Order(Ordered.HIGHEST_PRECEDENCE)
@Slf4j
public class RequestFilter implements WebFilter {


    /**
     * Get the request ID from the custom header "X-Request-Id" if present, otherwise it generates one.
     * Set the X-Request-Id value in the {@code response} and in the MDC
     */
    @Override
    public Mono<Void> filter(ServerWebExchange serverWebExchange,
                             WebFilterChain webFilterChain) {

        // get requestId from header or generate one
        var requestIdHeader = serverWebExchange.getRequest().getHeaders().get(HEADER_REQUEST_ID);
        String requestId;
        if (requestIdHeader == null || requestIdHeader.isEmpty()) {
            requestId = UUID.randomUUID().toString();
        } else {
            requestId = requestIdHeader.get(0);
        }

        // set requestId in MDC
        MDC.put("requestId", requestId);
        log.debug("{} {}", serverWebExchange.getRequest().getMethod(), serverWebExchange.getRequest().getURI());

        serverWebExchange.getResponse()
                .getHeaders().add(HEADER_REQUEST_ID, requestId);
        return webFilterChain.filter(serverWebExchange);
    }


}
