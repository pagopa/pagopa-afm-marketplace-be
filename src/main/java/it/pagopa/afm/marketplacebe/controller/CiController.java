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
import it.pagopa.afm.marketplacebe.model.bundle.BundleAttributeResponse;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsAttributes;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundleDetails;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundles;
import it.pagopa.afm.marketplacebe.model.offer.BundleCiOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiBundleId;
import it.pagopa.afm.marketplacebe.model.request.BundleRequestId;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.PublicBundleRequests;
import it.pagopa.afm.marketplacebe.service.BundleOfferService;
import it.pagopa.afm.marketplacebe.service.BundleRequestService;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import javax.validation.constraints.Size;
import java.util.List;

@RestController()
@RequestMapping(path = "/cis")
@Tag(name = "CI", description = "Everything about CI")
public class CiController {

    private final BundleRequestService bundleRequestService;

    private final BundleOfferService bundleOfferService;

    private final BundleService bundleService;

    @Autowired
    public CiController(BundleRequestService bundleRequestService, BundleOfferService bundleOfferService, BundleService bundleService) {
        this.bundleRequestService = bundleRequestService;
        this.bundleOfferService = bundleOfferService;
        this.bundleService = bundleService;
    }

    @Operation(summary = "Get paginated list of bundles of a CI", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiBundles.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/{ci-fiscal-code}/bundles", produces = {MediaType.APPLICATION_JSON_VALUE})
    public CiBundles getBundlesByFiscalCode(
            @Parameter(description = "Creditor institution's tax code", required = true) @PathVariable("ci-fiscal-code") String ciTaxCode,
            @Parameter(description = "Filtering the ciBundles by type") @RequestParam(required = false) BundleType type,
            @Parameter(description = "Filtering the ciBundles by bundle name") @RequestParam(required = false) String bundleName,
            @Parameter(description = "Filtering the ciBundles by pspBusinessName of the corresponding bundle") @RequestParam(required = false) String pspBusinessName,
            @Parameter(description = "Number of items for page") @RequestParam(required = false, defaultValue = "50") @Positive Integer limit,
            @Parameter(description = "Page number") @RequestParam(required = false, defaultValue = "0") @Min(0) @PositiveOrZero Integer page
    ) {
        return bundleService.getBundlesByFiscalCode(ciTaxCode, type, bundleName, pspBusinessName, limit, page);
    }

    @Operation(summary = "Get a bundle of a CI", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiBundleDetails.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{ci-fiscal-code}/bundles/{id-bundle}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public CiBundleDetails getBundleByFiscalCode(
            @Parameter(description = "Creditor institution's tax code", required = true) @PathVariable("ci-fiscal-code") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("id-bundle") String idBundle) {
        return bundleService.getBundleByFiscalCode(fiscalCode, idBundle);
    }

    @Operation(summary = "Remove a bundle of a CI", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(
            value = "/{ci-fiscal-code}/bundles/{idcibundle}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Void> removeBundleByFiscalCode(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String fiscalCode,
            @Parameter(description = "CIBundle identifier", required = true) @PathVariable("idcibundle") String idCiBundle) {
        bundleService.removeBundleByFiscalCode(fiscalCode, idCiBundle);
        return ResponseEntity.ok().build();
    }

    @Operation(summary = "Get attributes of a bundle of a CI", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleDetailsAttributes.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{ci-fiscal-code}/bundles/{id-bundle}/attributes",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public BundleDetailsAttributes getBundleAttributesByFiscalCode(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("id-bundle") String idBundle) {
        return bundleService.getBundleAttributesByFiscalCode(fiscalCode, idBundle);
    }

    @Operation(summary = "Create a new bundle attribute", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleAttributeResponse.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{ci-fiscal-code}/bundles/{id-bundle}/attributes",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleAttributeResponse> createBundleAttributesByCi(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("id-bundle") String idBundle,
            @RequestBody @Valid @NotNull CiBundleAttributeModel bundleAttribute) {
        return ResponseEntity.status(HttpStatus.CREATED).body(bundleService.createBundleAttributesByCi(fiscalCode, idBundle, bundleAttribute));
    }

    @Operation(summary = "Update an attribute of a bundle", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PutMapping(value = "/{ci-fiscal-code}/bundles/{id-bundle}/attributes/{idattribute}")
    public ResponseEntity<Void> updateBundleAttributesByCi(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("id-bundle") String idBundle,
            @Parameter(description = "Attribute identifier", required = true) @PathVariable("idattribute") String idAttribute,
            @RequestBody @Valid @NotNull CiBundleAttributeModel bundleAttribute) {
        bundleService.updateBundleAttributesByCi(fiscalCode, idBundle, idAttribute, bundleAttribute);
        return ResponseEntity.ok().build();
    }

    @Operation(summary = "Delete an attribute of a bundle", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(value = "/{ci-fiscal-code}/bundles/{id-bundle}/attributes/{idattribute}")
    public ResponseEntity<Void> removeBundleAttributesByCi(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("id-bundle") String idBundle,
            @Parameter(description = "Attribute identifier", required = true) @PathVariable("idattribute") String idAttribute) {
        bundleService.removeBundleAttributesByCi(fiscalCode, idBundle, idAttribute);
        return ResponseEntity.ok().build();
    }

    /**
     * GET /cis/:ci-fiscal-code/requests : Get paginated list of CI requests to the PSP regarding public bundles
     *
     * @param ciFiscalCode Creditor institution's tax code
     * @param idPsp        Payment service provider identifier
     * @param idBundle     bundle's identifier
     * @param limit        Number of element in the requested page
     * @param page         Page number
     * @return the paginated list of CI request to the PSP regarding public bundles
     */
    @Operation(summary = "Get paginated list of CI request to the PSP regarding public bundles", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PublicBundleRequests.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{ci-fiscal-code}/requests",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<PublicBundleRequests> getRequestsByCI(
            @Parameter(description = "Creditor institution's tax code", required = true) @PathVariable("ci-fiscal-code") String ciFiscalCode,
            @Parameter(description = "Filter by psp identifier") @RequestParam(required = false) @Size(max = 35) String idPsp,
            @Parameter(description = "Filter by bundle id") @RequestParam(required = false) String idBundle,
            @Parameter(description = "Number of items for page") @RequestParam(required = false, defaultValue = "50") @Positive @Max(100) Integer limit,
            @Parameter(description = "Page number") @RequestParam(required = false, defaultValue = "0") @PositiveOrZero @Min(0) @Max(10000) Integer page
    ) {
        return ResponseEntity.ok(bundleRequestService.getPublicBundleRequests(idPsp, limit, page, ciFiscalCode, idBundle));
    }

    /**
     * POST /cis/:ci-fiscal-code/requests : Create CI request to the PSP regarding public bundles
     *
     * @param ciFiscalCode CI identifier.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Create CI request to the PSP regarding public bundles", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleRequestId.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "409", description = "Conflict", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{ci-fiscal-code}/requests",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleRequestId> createRequest(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String ciFiscalCode,
            @RequestBody @Valid @NotNull CiBundleSubscriptionRequest ciBundleSubscriptionRequest) {
        return ResponseEntity.status(HttpStatus.CREATED).body(bundleRequestService.createBundleRequest(ciFiscalCode, ciBundleSubscriptionRequest));
    }

    /**
     * DELETE /cis/:ci-fiscal-code/requests/:id-bundle-request : Delete CI request regarding public bundles
     *
     * @param ciFiscalCode CI identifier.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Delete CI request regarding a public bundles", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(
            value = "/{ci-fiscal-code}/requests/{id-bundle-request}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Void> removeRequest(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String ciFiscalCode,
            @Parameter(description = "CI identifier", required = true) @PathVariable("id-bundle-request") String idBundleRequest) {
        bundleRequestService.removeBundleRequest(ciFiscalCode, idBundleRequest);
        return ResponseEntity.ok().build();
    }

    /**
     * GET /cis/:ci-tax-code/offers : Get paginated list of PSP offers to the CI regarding private bundles
     *
     * @param ciTaxCode CI identifier.
     * @param idPsp     PSP identifier. Optional filter.
     * @param limit     Number of element in the requested page
     * @param page      Page number
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get paginated list of PSP offers to the CI regarding private bundles", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleCiOffers.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/{ci-fiscal-code}/offers", produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity<BundleCiOffers> getOffersByCI(
            @Parameter(description = "Tax code of the creditor institution to which the offers are addressed", required = true) @PathVariable("ci-fiscal-code") String ciTaxCode,
            @Parameter(description = "Id of the payment service provider that has created the offers (used for to filter out the result)") @RequestParam(required = false) String idPsp,
            @Parameter(description = "Filtering the offers by bundle name") @RequestParam(required = false) String bundleName,
            @Parameter(description = "Number of items for page") @RequestParam(required = false, defaultValue = "50") @Positive Integer limit,
            @Parameter(description = "Page number") @RequestParam(required = false, defaultValue = "0") @Min(0) @PositiveOrZero Integer page
    ) {
        return ResponseEntity.ok(this.bundleOfferService.getCiOffers(ciTaxCode, idPsp, bundleName, limit, page));
    }

    @Operation(summary = "The CI accepts an offer of a PSP", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiBundleId.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{ci-fiscal-code}/offers/{id-bundle-offer}/accept"
    )
    public ResponseEntity<CiBundleId> acceptOffer(
            @Parameter(description = "Creditor institution's tax code", required = true) @PathVariable("ci-fiscal-code") String ciFiscalCode,
            @Parameter(description = "Bundle offer identifier", required = true) @PathVariable("id-bundle-offer") String idBundleOffer,
            @RequestBody @Valid @NotNull List<CiBundleAttributeModel> bundleAttributes
    ) {
        return ResponseEntity.status(HttpStatus.CREATED).body(this.bundleOfferService.acceptOffer(ciFiscalCode, idBundleOffer, bundleAttributes));
    }

    @Operation(summary = "The CI rejects the offer of the PSP", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{ci-fiscal-code}/offers/{id-bundle-offer}/reject"
    )
    public ResponseEntity<Void> rejectOffer(
            @Parameter(description = "CI identifier", required = true) @PathVariable("ci-fiscal-code") String ciFiscalCode,
            @Parameter(description = "Bundle offer identifier", required = true) @PathVariable("id-bundle-offer") String idBundleOffer) {
        bundleOfferService.rejectOffer(ciFiscalCode, idBundleOffer);
        return ResponseEntity.ok().build();
    }
}
