package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.model.AppInfo;
import it.pagopa.afm.marketplacebe.model.CalculatorConfiguration;
import it.pagopa.afm.marketplacebe.model.ProblemJson;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.service.BundleService;
import it.pagopa.afm.marketplacebe.task.CalculatorTaskExecutor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.view.RedirectView;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.List;

@RestController
public class HomeController {

    @Value("${application.name}")
    private String name;

    @Value("${application.version}")
    private String version;

    @Value("${properties.environment}")
    private String environment;

    @Value("${server.servlet.context-path}")
    private String basePath;

    @Autowired
    private BundleService bundleService;

    /**
     * @return redirect to Swagger page documentation
     */
    @Hidden
    @GetMapping("")
    public RedirectView home() {
        return new RedirectView(basePath + "/swagger-ui.html");
    }

    /**
     * Health Check
     *
     * @return ok
     */
    @Operation(summary = "Return OK if application is started", tags = {"Home"})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = AppInfo.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "403", description = "Forbidden", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})

    @GetMapping("/info")
    public ResponseEntity<AppInfo> healthCheck() {

        AppInfo info = AppInfo.builder()
                .name(name)
                .version(version)
                .environment(environment)
                .build();
        return ResponseEntity.status(HttpStatus.OK).body(info);
    }

    @Operation(summary = "Get bundles by type", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Bundles.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/bundles",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public Bundles getGlobalBundles(
            @Positive @Parameter(description = "Number of items for page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @PositiveOrZero @Parameter(description = "Page number. Page number value starts from 0. Default = 1") @RequestParam(required = false, defaultValue = "1") Integer page,
            @Parameter(description = "Bundle type. Default = GLOBAL") @RequestParam(required = false, defaultValue = "GLOBAL") @Valid List<BundleType> types) {
        return bundleService.getBundles(types);
    }

    @Operation(summary = "Get the configuration", tags = {"Home",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CalculatorConfiguration.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/configuration",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public CalculatorConfiguration getConfiguration() {
        return bundleService.getConfiguration();
    }
}
