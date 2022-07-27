package it.pagopa.afm.marketplacebe.service;

import feign.FeignException;
import it.pagopa.afm.marketplacebe.config.FeignConfig;
import it.pagopa.afm.marketplacebe.model.CalculatorConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(value = "calculator", url = "${service.calculator.url}", configuration = FeignConfig.class)
public interface CalculatorService {

    @Retryable(exclude = FeignException.FeignClientException.class,
            maxAttemptsExpression = "${service.calculator.retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${service.calculator.retry.maxDelay}"))
    @PostMapping(value = "/configure", consumes = MediaType.APPLICATION_JSON_VALUE)
    void configure(@RequestBody CalculatorConfiguration configuration);
}
