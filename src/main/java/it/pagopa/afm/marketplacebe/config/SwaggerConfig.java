package it.pagopa.afm.marketplacebe.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springdoc.core.customizers.OpenApiCustomiser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

import static it.pagopa.afm.marketplacebe.util.Constants.HEADER_REQUEST_ID;

@Configuration
public class SwaggerConfig {

    @Bean
    OpenAPI customOpenAPI(@Value("${info.app.name}") String appTitle,
                          @Value("${info.app.description}") String appDescription,
                          @Value("${info.app.version}") String appVersion) {
        return new OpenAPI()
                .components(new Components()
                        .addSecuritySchemes("ApiKey", new SecurityScheme()
                                .type(SecurityScheme.Type.APIKEY)
                                .description("The API key to access this function app.")
                                .name("Ocp-Apim-Subscription-Key")
                                .in(SecurityScheme.In.HEADER))
                )
                .info(new Info()
                        .title(appDescription)
                        .version(appVersion)
                        .description(appTitle)
                        .termsOfService("https://www.pagopa.gov.it/"));
    }
    @Bean
    OpenApiCustomiser sortOperationsAlphabetically() {
        return openApi -> {
            Paths paths = openApi.getPaths().entrySet()
                    .stream()
                    .sorted(Map.Entry.comparingByKey())
                    .collect(Paths::new, (map, item) -> map.addPathItem(item.getKey(), item.getValue()), Paths::putAll);

            paths.forEach((key, value) -> value.readOperations().forEach(operation -> {
                var responses = operation.getResponses().entrySet().stream()
                        .sorted(Map.Entry.comparingByKey())
                        .collect(ApiResponses::new, (map, item) -> map.addApiResponse(item.getKey(), item.getValue()), ApiResponses::putAll);
                operation.setResponses(responses);
            }));
            openApi.setPaths(paths);
        };
    }
    @Bean
    public OpenApiCustomiser addCommonHeaders() {
        return openApi -> openApi.getPaths().forEach((key, value) -> {

            // add Request-ID as request header
            value.addParametersItem(new Parameter().in("header")
                    .name(HEADER_REQUEST_ID)
                    .schema(new StringSchema())
                    .description("This header identifies the call, if not passed it is self-generated. This ID is returned in the response."));

            // add Request-ID as response header
            value.readOperations()
                    .forEach(operation -> operation.getResponses().values()
                            .forEach(response -> response.addHeaderObject(HEADER_REQUEST_ID, new Header()
                                    .schema(new StringSchema())
                                    .description("This header identifies the call"))));
        });
    }
}
