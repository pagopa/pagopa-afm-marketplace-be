package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import it.pagopa.afm.marketplacebe.model.AppInfo;
import it.pagopa.afm.marketplacebe.model.ProblemJson;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.view.RedirectView;

@RestController
public class HomeController {

    private final String name;

    private final String version;

    private final String environment;

    private final BundleService bundleService;

    @Autowired
    public HomeController(
            @Value("${info.app.name}") String name,
            @Value("${info.app.version}") String version,
            @Value("${properties.environment}") String environment,
            BundleService bundleService
    ) {
        this.name = name;
        this.version = version;
        this.environment = environment;
        this.bundleService = bundleService;
    }

    /**
     * @return redirect to Swagger page documentation
     */
    @Hidden
    @GetMapping("")
    public RedirectView home() {
        return new RedirectView("/swagger-ui.html");
    }

    /**
     * Health Check
     *
     * @return ok
     */
    @Operation(summary = "Return OK if application is started", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"Home"})
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

    @Operation(summary = "Generate the configuration", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"Home",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/configuration",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Void> getConfiguration() {
        bundleService.getConfiguration();
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
