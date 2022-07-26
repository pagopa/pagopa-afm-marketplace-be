package it.pagopa.afm.marketplacebe.service;

import feign.FeignException;
import it.pagopa.afm.marketplacebe.config.FeignConfig;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.PostMapping;

@FeignClient(value = "calculator", url = "${service.calculator.url}", configuration = FeignConfig.class)
public interface CalculatorService {

    @Retryable(exclude = FeignException.FeignClientException.class,
            maxAttemptsExpression = "${service.calculator.retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${service.calculator.retry.maxDelay}"))
    @PostMapping(value = "/configure")
    void configure();
}
