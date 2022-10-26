package it.pagopa.afm.marketplacebe;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.retry.annotation.EnableRetry;

import javax.validation.Validator;

@SpringBootApplication
@EnableRetry
public class MarketplaceBeApplication {

    public static void main(String[] args) {
        SpringApplication.run(MarketplaceBeApplication.class, args);
    }

    @Bean
    public Validator validator() {
        return new org.springframework.validation.beanvalidation.LocalValidatorFactoryBean();
    }

}

