package it.pagopa.afm.marketplacebe.config;

import com.azure.cosmos.implementation.Utils;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

import javax.annotation.PostConstruct;


@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
public class ObjectMapperConfiguration {

    // used for deserialization
    // to avoid the following error:
    //      Caused by: com.fasterxml.jackson.databind.exc.InvalidDefinitionException:
    //      Java 8 date/time type `java.time.LocalDate` not supported by default:
    //      add Module "com.fasterxml.jackson.datatype:jackson-datatype-jsr310" to enable handling
    @PostConstruct
    public void configureCosmosSimpleObjectMapper() {
        Utils
                .getSimpleObjectMapper()
                .registerModule(new JavaTimeModule());
    }

}
