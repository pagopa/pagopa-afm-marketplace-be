package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.pagopa.afm.marketplacebe.entity.BundleRequest;
import it.pagopa.afm.marketplacebe.model.ProblemJson;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffered;
import it.pagopa.afm.marketplacebe.model.offer.BundleOffers;
import it.pagopa.afm.marketplacebe.model.offer.CiFiscalCodeList;
import it.pagopa.afm.marketplacebe.model.request.Requests;
import it.pagopa.afm.marketplacebe.service.BundleOfferService;
import it.pagopa.afm.marketplacebe.service.BundleRequestService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

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
    private BundleOfferService bundleOfferService;

    @Autowired
    private BundleRequestService bundleRequestService;

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
    public BundleOffers getOffers(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Positive @Parameter(description = "Number of items for page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @PositiveOrZero @Parameter(description = "Page number. Page number value starts from 0. Default = 1") @RequestParam(required = false, defaultValue = "1") Integer page) {
//        return bundleOfferService.getPspOffers(idPsp, limit, page);
        return bundleOfferService.getPspOffers(idPsp);
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
        return ResponseEntity.ok(bundleOfferService.sendBundleOffer(idPsp, idBundle, ciFiscalCodeList));
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
     * @param idPsp  PSP identifier.
     * @param size   Number of elements for page. Default = 50.
     * @param cursor Cursor from which starts counting.
     * @return OK. (status code 200)
     * or Service unavailable (status code 500)
     */
    @Operation(summary = "Get paginated list of CI request to the PSP regarding public bundles", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Requests.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/requests",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public Mono<Requests> getRequests(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Positive @Parameter(description = "Number of elements for one page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer size,
            @Parameter(description = "Starting cursor") @RequestParam(required = false) String cursor,
            @Parameter(description = "Filter by creditor institution") @RequestParam(required = false) String ciFiscalCode) {
        return bundleRequestService.getRequests(idPsp, size, cursor, ciFiscalCode);
    }


    @Operation(summary = "the PSP accepts a request of a CI", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/requests/{idBundleRequest}/accept",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public Mono<Void> acceptRequest(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Size(max = 35) @Parameter(description = "Bundle Request identifier", required = true) @PathVariable("idBundleRequest") String idBundleRequest) {
        return bundleRequestService.acceptRequest(idPsp, idBundleRequest);
    }


    @Operation(summary = "the PSP rejects a request of a CI", tags = {"PSP",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{idpsp}/requests/{idBundleRequest}/reject",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public Mono<Void> rejectRequest(
            @Size(max = 35) @Parameter(description = "PSP identifier", required = true) @PathVariable("idpsp") String idPsp,
            @Size(max = 35) @Parameter(description = "Bundle Request identifier", required = true) @PathVariable("idBundleRequest") String idBundleRequest) {
       return  bundleRequestService.rejectRequest(idPsp, idBundleRequest);
    }


}
