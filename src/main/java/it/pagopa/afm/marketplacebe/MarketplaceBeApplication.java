package it.pagopa.afm.marketplacebe;

import com.azure.spring.data.cosmos.core.mapping.EnableCosmosAuditing;
import com.azure.spring.data.cosmos.repository.config.EnableReactiveCosmosRepositories;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
@EnableCosmosAuditing
@EnableReactiveCosmosRepositories("it.pagopa.afm.marketplacebe.repository")
public class MarketplaceBeApplication {

    public static void main(String[] args) {
        SpringApplication.run(MarketplaceBeApplication.class, args);
    }

}

