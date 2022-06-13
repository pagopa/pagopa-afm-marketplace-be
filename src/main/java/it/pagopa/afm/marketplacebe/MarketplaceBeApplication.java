package it.pagopa.afm.marketplacebe;

import com.azure.spring.data.cosmos.core.mapping.EnableCosmosAuditing;
import com.azure.spring.data.cosmos.repository.config.EnableCosmosRepositories;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
@EnableCosmosRepositories("it.gov.pagopa.afm.marketplace.repository")
@EnableCosmosAuditing
public class MarketplaceBeApplication {

	public static void main(String[] args) {
		SpringApplication.run(MarketplaceBeApplication.class, args);
	}

}
