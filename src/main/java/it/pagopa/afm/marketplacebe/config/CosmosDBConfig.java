package it.pagopa.afm.marketplacebe.config;

import com.azure.cosmos.CosmosClientBuilder;
import com.azure.spring.data.cosmos.config.AbstractCosmosConfiguration;
import com.azure.spring.data.cosmos.config.CosmosConfig;
import com.azure.spring.data.cosmos.repository.config.EnableReactiveCosmosRepositories;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableReactiveCosmosRepositories
public class CosmosDBConfig extends AbstractCosmosConfiguration {
    @Value("${azure.cosmos.uri}")
    private String cosmosUri;

    @Value("${azure.cosmos.key")
    private String cosmosKey;

    @Value("${azure.cosmos.database")
    private String cosmosName;


    @Bean
    public CosmosClientBuilder appCosmosClientBuilder() {
        return new CosmosClientBuilder()
                .key(cosmosKey)
                .endpoint(cosmosUri);
    }

    @Bean
    public CosmosConfig cosmosConfig() {
        return CosmosConfig.builder()
                .enableQueryMetrics(true)
                .build();
    }

    @Override
    protected String getDatabaseName() {
        return cosmosName;
    }
}
