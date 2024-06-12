package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.pagopa.afm.marketplacebe.entity.BundleType;
import it.pagopa.afm.marketplacebe.model.ProblemJson;
import it.pagopa.afm.marketplacebe.model.bundle.Bundles;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.Min;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping(value = "/bundles", produces = MediaType.APPLICATION_JSON_VALUE)
@Tag(name = "Bundle")
public class BundleController {

    private final BundleService bundleService;

    @Autowired
    public BundleController(BundleService bundleService) {
        this.bundleService = bundleService;
    }

    /**
     * Retrieve the paginated list of bundles given the type, name and valid date
     *
     * @param types     list of bundle's type
     * @param name      bundle's name
     * @param validFrom validity date of bundles, used to retrieve all bundles valid from the specified date
     * @param limit     page size
     * @param page      page number
     * @return a paginated list of bundles
     */
    @Operation(summary = "Get paginated list of bundles", security = {@SecurityRequirement(name = "ApiKey")})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Bundles.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "", produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity<Bundles> getGlobalBundles(
            @Parameter(description = "Bundle's type") @RequestParam(required = false, defaultValue = "GLOBAL") @Valid List<BundleType> types,
            @Parameter(description = "Bundle's name") @RequestParam(required = false) @Valid String name,
            @Parameter(description = "Validity date of bundles, used to retrieve all bundles valid from the specified date (yyyy-MM-dd)", example = "2024-05-10") @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate validFrom,
            @Parameter(description = "Validity date of bundles, used to retrieve all bundles that expire at the specified date (yyyy-MM-dd)", example = "2024-05-10") @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate expireAt,
            @Parameter(description = "Number of items for page") @RequestParam(required = false, defaultValue = "50") @Positive Integer limit,
            @Parameter(description = "Page number") @RequestParam(required = false, defaultValue = "0") @Min(0) @PositiveOrZero Integer page
    ) {
        return ResponseEntity.ok(this.bundleService.getBundles(types, name, validFrom, expireAt, limit, page));
    }

    /**
     * Retrieve the bundle details
     *
     * @param idBundle Bundle identifier
     * @return the bundle details
     */
    @Operation(summary = "Get the bundle details", security = {@SecurityRequirement(name = "ApiKey")})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PspBundleDetails.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/{id-bundle}", produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity<PspBundleDetails> getBundleDetails(
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("id-bundle") String idBundle
    ) {
        return ResponseEntity.ok(this.bundleService.getBundleDetailsById(idBundle));
    }
}
