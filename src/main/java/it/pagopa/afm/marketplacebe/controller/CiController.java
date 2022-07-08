package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.pagopa.afm.marketplacebe.model.ProblemJson;
import it.pagopa.afm.marketplacebe.model.bundle.BundleAttributeResponse;
import it.pagopa.afm.marketplacebe.model.bundle.BundleDetailsAttributes;
import it.pagopa.afm.marketplacebe.model.bundle.CiBundles;
import it.pagopa.afm.marketplacebe.model.bundle.PspBundleDetails;
import it.pagopa.afm.marketplacebe.model.offer.BundleCiOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiBundleId;
import it.pagopa.afm.marketplacebe.model.request.BundleRequestId;
import it.pagopa.afm.marketplacebe.model.request.CiBundleAttributeModel;
import it.pagopa.afm.marketplacebe.model.request.CiBundleSubscriptionRequest;
import it.pagopa.afm.marketplacebe.model.request.CiRequests;
import it.pagopa.afm.marketplacebe.service.BundleOfferService;
import it.pagopa.afm.marketplacebe.service.BundleRequestService;
import it.pagopa.afm.marketplacebe.service.BundleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

@RestController()
@RequestMapping(path = "/cis")
@Tag(name = "CI", description = "Everything about CI")
public class CiController {

    @Autowired
    private BundleRequestService bundleRequestService;

    @Autowired
    private BundleOfferService bundleOfferService;

    @Autowired
    private BundleService bundleService;

    @Operation(summary = "Get paginated list of bundles of a CI", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiBundles.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{cifiscalcode}/bundles",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public CiBundles getBundlesByFiscalCode(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String fiscalCode,
            @Positive @Parameter(description = "Number of items for page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @PositiveOrZero @Parameter(description = "Page number. Page number value starts from 0. Default = 1") @RequestParam(required = false, defaultValue = "1") Integer page) {

        return bundleService.getBundlesByFiscalCode(fiscalCode, limit, page);
    }

    @Operation(summary = "Get a bundle of a CI", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PspBundleDetails.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{cifiscalcode}/bundles/{idbundle}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public PspBundleDetails getBundleByFiscalCode(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle) {
        return bundleService.getBundleByFiscalCode(fiscalCode, idBundle);
    }

    @Operation(summary = "Remove a bundle of a CI", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(
            value = "/{cifiscalcode}/bundles/{idcibundle}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Void> removeBundleByFiscalCode(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String fiscalCode,
            @Parameter(description = "CIBundle identifier", required = true) @PathVariable("idcibundle") String idCiBundle) {
        bundleService.removeBundleByFiscalCode(fiscalCode, idCiBundle);
        return ResponseEntity.ok().build();
    }

    @Operation(summary = "Get attributes of a bundle of a CI", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleDetailsAttributes.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{cifiscalcode}/bundles/{idbundle}/attributes",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public BundleDetailsAttributes getBundleAttributesByFiscalCode(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle) {
        return bundleService.getBundleAttributesByFiscalCode(fiscalCode, idBundle);
    }

    @Operation(summary = "Create a new bundle attribute", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleAttributeResponse.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{cifiscalcode}/bundles/{idbundle}/attributes",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleAttributeResponse> createBundleAttributesByCi(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle,
            @RequestBody @Valid @NotNull CiBundleAttributeModel bundleAttribute) {
        return ResponseEntity.status(HttpStatus.CREATED).body(bundleService.createBundleAttributesByCi(fiscalCode, idBundle, bundleAttribute));
    }

    @Operation(summary = "Update an attribute of a bundle", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PutMapping(value = "/{cifiscalcode}/bundles/{idbundle}/attributes/{idattribute}")
    public ResponseEntity<Void> updateBundleAttributesByCi(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle,
            @Parameter(description = "Attribute identifier", required = true) @PathVariable("idattribute") String idAttribute,
            @RequestBody @Valid @NotNull CiBundleAttributeModel bundleAttribute) {
        bundleService.updateBundleAttributesByCi(fiscalCode, idBundle, idAttribute, bundleAttribute);
        return ResponseEntity.ok().build();
    }

    @Operation(summary = "Delete an attribute of a bundle", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(value = "/{cifiscalcode}/bundles/{idbundle}/attributes/{idattribute}")
    public ResponseEntity<Void> removeBundleAttributesByCi(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String fiscalCode,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle,
            @Parameter(description = "Attribute identifier", required = true) @PathVariable("idattribute") String idAttribute) {
        bundleService.removeBundleAttributesByCi(fiscalCode, idBundle, idAttribute);
        return ResponseEntity.ok().build();
    }

    /**
     * GET /cis/:cifiscalcode/requests : Get paginated list of CI requests to the PSP regarding public bundles
     *
     * @param ciFiscalCode CI identifier.
     * @param size         Number of elements for page. Default = 50.
     * @param cursor       Cursor from which starts counting.
     * @param idPsp        PSP identifier. Optional filter.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get paginated list of CI request to the PSP regarding public bundles", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiRequests.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{cifiscalcode}/requests",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<CiRequests> getRequestsByCI(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String ciFiscalCode,
            @Positive @Parameter(description = "Number of elements for one page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer size,
            @Parameter(description = "Starting cursor") @RequestParam(required = false) String cursor,
            @Parameter(description = "Filter by psp") @RequestParam(required = false) String idPsp) {
        return ResponseEntity.ok(bundleRequestService.getRequestsByCI(ciFiscalCode, size, cursor, idPsp));

    }

    /**
     * POST /cis/:cifiscalcode/requests : Create CI request to the PSP regarding public bundles
     *
     * @param ciFiscalCode CI identifier.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Create CI request to the PSP regarding public bundles", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleRequestId.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{cifiscalcode}/requests",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleRequestId> createRequest(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String ciFiscalCode,
            @RequestBody @Valid @NotNull CiBundleSubscriptionRequest ciBundleSubscriptionRequest) {
        return ResponseEntity.status(HttpStatus.CREATED).body(bundleRequestService.createBundleRequest(ciFiscalCode, ciBundleSubscriptionRequest));
    }

    /**
     * DELETE /cis/:cifiscalcode/requests/:idbundlerequest : Delete CI request regarding public bundles
     *
     * @param ciFiscalCode CI identifier.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Delete CI request regarding a public bundles", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(
            value = "/{cifiscalcode}/requests/{idbundlerequest}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Void> removeRequest(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String ciFiscalCode,
            @Parameter(description = "CI identifier", required = true) @PathVariable("idbundlerequest") String idBundleRequest) {
        bundleRequestService.removeBundleRequest(ciFiscalCode, idBundleRequest);
        return ResponseEntity.ok().build();
    }

    /**
     * GET /cis/:cifiscalcode/offers : Get paginated list of PSP offers to the CI regarding private bundles
     *
     * @param ciFiscalCode CI identifier.
     * @param size         Number of elements for page. Default = 50.
     * @param cursor       Cursor from which starts counting.
     * @param idPsp        PSP identifier. Optional filter.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get paginated list of PSP offers to the CI regarding private bundles", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleCiOffers.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{cifiscalcode}/offers",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleCiOffers> getOffersByCI(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String ciFiscalCode,
            @Positive @Parameter(description = "Number of elements for one page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer size,
            @Parameter(description = "Starting cursor") @RequestParam(required = false) String cursor,
            @Parameter(description = "Filter by psp") @RequestParam(required = false) String idPsp) {
        return ResponseEntity.ok(bundleOfferService.getCiOffers(ciFiscalCode, size, cursor, idPsp));
    }

    @Operation(summary = "The CI accepts an offer of a PSP", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiBundleId.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{cifiscalcode}/offers/{idbundleoffer}/accept"
    )
    public ResponseEntity<CiBundleId> acceptOffer(
            @Parameter(description = "PSP identifier", required = true) @PathVariable("cifiscalcode") String ciFiscalCode,
            @Parameter(description = "Bundle offer identifier", required = true) @PathVariable("idbundleoffer") String idBundleOffer) {
        return ResponseEntity.status(HttpStatus.CREATED).body(bundleOfferService.acceptOffer(ciFiscalCode, idBundleOffer));
    }

    @Operation(summary = "The CI rejects the offer of the PSP", tags = {"CI",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{cifiscalcode}/offers/{idbundleoffer}/reject"
    )
    public ResponseEntity<Void> rejectOffer(
            @Parameter(description = "CI identifier", required = true) @PathVariable("cifiscalcode") String ciFiscalCode,
            @Parameter(description = "Bundle offer identifier", required = true) @PathVariable("idbundleoffer") String idBundleOffer) {
        bundleOfferService.rejectOffer(ciFiscalCode, idBundleOffer);
        return ResponseEntity.ok().build();
    }
}
