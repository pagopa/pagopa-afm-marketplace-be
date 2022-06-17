package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.pagopa.afm.marketplacebe.model.ProblemJson;
import it.pagopa.afm.marketplacebe.model.bundle.*;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.request.PspRequests;
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
import javax.validation.constraints.Size;
import java.util.List;

@RestController()
@RequestMapping(path = "/psps")
@Tag(name = "PSP", description = "Everything about PSP")
public class PspController {

    @Autowired
    private BundleService bundleService;

    @Autowired
    private BundleOfferService bundleOfferService;

    @Autowired
    private BundleRequestService bundleRequestService;

    /**
     * GET /psps/:idpsp/bundles : Get bundle list of the given PSP
     *
     * @param idPsp : PSP identifier
     * @return ResponseEntity with status 200 (OK) and with body the bundle list
     */
    @Operation(summary = "Get paginated list of bundles of a PSP", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Bundles.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/bundles",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Bundles> getBundles(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Positive @Parameter(description = "Number of items for page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @PositiveOrZero @Parameter(description = "Page number. Page number value starts from 0. Default = 1") @RequestParam(required = false, defaultValue = "1") Integer page) {
        return ResponseEntity.ok(bundleService.getBundlesByIdPsp(idPsp, 0, 100));
    }


    /**
     * GET /psps/:idpsp/bundle/:idbundle : Get a bundle
     *
     * @param idPsp : PSP identifier
     * @param idBundle : Bundle identifier
     * @return
     */
    @Operation(summary = "Get a bundle", security = {}, tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleDetails.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/bundles/{idbundle}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleDetails> getBundle(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Size(max = 35) @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle){
        return ResponseEntity.ok(bundleService.getBundleById(idBundle, idPsp));
    }

    /**
     * GET /psps/:idpsp/bundle/:idbundle/creditorInstitutions : Get CIs subscribed to a bundle
     *
     * @param idPsp : PSP identifier
     * @param idBundle : Bundle identifier
     * @return
     */
    @Operation(summary = "Get paginated list of CI subscribed to a bundle", security = {}, tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE , schema = @Schema(implementation = CiFiscalCodeList.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/bundles/{idbundle}/creditorInstitutions",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<CiFiscalCodeList> getBundleCreditorInstitutions(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle){
        // TODO: return pagination
        return ResponseEntity.ok(bundleService.getCIs(idBundle, idPsp));
    }

    /**
     * GET /psps/:idpsp/bundle/:idbundle/creditorInstitutions/:cifiscalcode : Get details of a relationship between a bundle and a creditor institution
     * institution
     *
     * @param idPsp : PSP identifier
     * @param idBundle : Bundle identifier
     * @param ciFiscalCode : Creditor Institution fiscal code
     * @return
     */
    @Operation(summary = "Get details of a relationship between a bundle and a creditor institution", security = {}, tags = {"PSP",})
    @ApiResponses(value = {
            // TODO: update schema - 200
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE , schema = @Schema(implementation = CiBundleDetails.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/bundles/{idbundle}/creditorInstitutions/{cifiscalcode}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<CiBundleDetails> getBundleCreditorInstitutionDetails(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Size(max = 35) @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle,
            @Size(max = 35) @Parameter(description = "Bundle identifier", required = true) @PathVariable("cifiscalcode") String ciFiscalCode){
        return ResponseEntity.ok(bundleService.getCIDetails(idBundle, idPsp, ciFiscalCode));
    }


    /**
     * POST /psps/:idpsp/bundles : Create a bundle
     *
     * @param idPsp : PSP identifier
     * @return
     */
    @Operation(summary = "Create a new bundle", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleResponse.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{idpsp}/bundles",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleResponse> createBundle(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @RequestBody @Valid @NotNull BundleRequest bundleRequest) {
        return ResponseEntity.status(HttpStatus.CREATED).body(bundleService.createBundle(idPsp, bundleRequest));
    }

    /**
     * UPDATE /psps/:idpsp/bundles/:idbundle : Update the bundle with the given id
     *
     * @param idPsp    : PSP identifier
     * @param idBundle : Bundle identifier
     * @return
     */
    @Operation(summary = "Update a bundle", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PutMapping(
            value = "/{idpsp}/bundles/{idbundle}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Void> updateBundle(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle,
            @RequestBody @Valid @NotNull BundleRequest bundleRequest) {
        bundleService.updateBundle(idPsp, idBundle, bundleRequest);
        return ResponseEntity.ok().build();
    }

    /**
     * DELETE /psps/:idpsp/bundles/:idbundle : Delete the bundle with the given id
     *
     * @param idPsp    : PSP identifier
     * @param idBundle : Bundle identifier
     * @return
     */
    @Operation(summary = "Delete a bundle", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema())),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(
            value = "/{idpsp}/bundles/{idbundle}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<Void> removeBundle(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle) {
        bundleService.removeBundle(idPsp, idBundle);
        return ResponseEntity.ok().build();
    }

    /**
     * GET /psps/:idpsp/offers : Get paginated list of PSP offers regarding private bundles
     *
     * @param idPsp PSP identifier.
     * @param limit Number of items for page. Default = 50.
     * @param page  Page number. Default = 0.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get paginated list of PSP offers regarding private bundles", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = BundleOffers.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/offers",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<BundleOffers> getOffers(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Positive @Parameter(description = "Number of items for page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @PositiveOrZero @Parameter(description = "Page number. Page number value starts from 0. Default = 1") @RequestParam(required = false, defaultValue = "1") Integer page) {
        return ResponseEntity.ok(bundleOfferService.getPspOffers(idPsp));
    }


    /**
     * POST /psps/:idpsp/bunldes/:idbundle/offers : PSP offers a private bundle to a creditor institution
     *
     * @param ciFiscalCodeList PSP identifier.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get cursored list of PSP offers regarding private bundles", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiFiscalCodeList.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{idpsp}/bundles/{idbundle}/offers",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<List<BundleOffered>> sendBundleOffer(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle,
            @RequestBody @Valid @NotNull CiFiscalCodeList ciFiscalCodeList) {
        return ResponseEntity.status(HttpStatus.CREATED).body(bundleOfferService.sendBundleOffer(idPsp, idBundle, ciFiscalCodeList));
    }

    /**
     * DELETE /psps/:idpsp/bunldes/:idbundle/offers/:idbundleoffer : PSP offers a private bundle to a creditor institution
     *
     * @param idPsp         PSP identifier.
     * @param idBundle      Bundle identifier.
     * @param idBundleOffer Bundle Offer identifier.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get cursored list of PSP offers regarding private bundles", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CiFiscalCodeList.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(
            value = "/{idpsp}/bundles/{idbundle}/offers/{idbundleoffer}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public ResponseEntity<List<BundleOffered>> removeBundleOffer(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Parameter(description = "Bundle identifier", required = true) @PathVariable("idbundle") String idBundle,
            @Parameter(description = "Bundle offer identifier", required = true) @PathVariable("idbundleoffer") String idBundleOffer) {
        bundleOfferService.removeBundleOffer(idPsp, idBundle, idBundleOffer);
        return ResponseEntity.ok().build();
    }


    /**
     * GET /psps/:idpsp/requests : Get paginated list of CI requests to the PSP regarding public bundles
     *
     * @param idPsp PSP identifier.
     * @param limit Number of elements for page. Default = 50.
     * @param page  Page number. Default = 1.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get paginated list of CI request to the PSP regarding public bundles", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PspRequests.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/requests",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public PspRequests getRequestsByPsp(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Positive @Parameter(description = "Number of items for page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @PositiveOrZero @Parameter(description = "Page number. Page number value starts from 0. Default = 1") @RequestParam(required = false, defaultValue = "1") Integer page,
            @PositiveOrZero @Parameter(description = "Cursor") @RequestParam(required = false) String cursor,
            @Parameter(description = "Filter by creditor institution") @RequestParam(required = false) String ciFiscalCode) {
        return bundleRequestService.getRequestsByPsp(idPsp, limit, page, cursor, ciFiscalCode);
    }


    @Operation(summary = "the PSP accepts a request of a CI", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{idpsp}/requests/{idBundleRequest}/accept"
    )
    public ResponseEntity<Void> acceptRequest(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Size(max = 35) @Parameter(description = "Bundle Request identifier", required = true) @PathVariable("idBundleRequest") String idBundleRequest) {
        bundleRequestService.acceptRequest(idPsp, idBundleRequest);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }


    @Operation(summary = "the PSP rejects a request of a CI", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(
            value = "/{idpsp}/requests/{idBundleRequest}/reject"
    )
    public ResponseEntity<Void> rejectRequest(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Size(max = 35) @Parameter(description = "Bundle Request identifier", required = true) @PathVariable("idBundleRequest") String idBundleRequest) {
        bundleRequestService.rejectRequest(idPsp, idBundleRequest);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }


}
