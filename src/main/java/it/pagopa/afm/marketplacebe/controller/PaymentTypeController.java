package it.pagopa.afm.marketplacebe.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import it.pagopa.afm.marketplacebe.model.ProblemJson;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentType;
import it.pagopa.afm.marketplacebe.model.paymenttype.PaymentTypes;
import it.pagopa.afm.marketplacebe.service.PaymentTypeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.List;

@RestController
@RequestMapping(path = "/paymenttypes")
public class PaymentTypeController {

    @Autowired
    private PaymentTypeService paymentTypeService;

    @Operation(summary = "Get payment types", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"Payment Type",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PaymentTypes.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public PaymentTypes getPaymentTypes(
            @Positive @Parameter(description = "Number of items for page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @PositiveOrZero @Parameter(description = "Page number. Page number value starts from 0. Default = 0") @RequestParam(required = false, defaultValue = "0") Integer page) {
        return paymentTypeService.getPaymentTypes();
    }

    @Operation(summary = "Get payment types", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"Payment Type",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PaymentType.class))),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(
            value = "/{id}",
            produces = {MediaType.APPLICATION_JSON_VALUE}
    )
    public PaymentType getPaymentType(
            @Parameter(description = "Payment type name", required = true) @PathVariable("id") String id) {
        return paymentTypeService.getPaymentType(id);
    }

//    @Operation(summary = "Upload a set of payment types by list", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"Payment Type",})
//    @ApiResponses(value = {
//            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = it.pagopa.afm.marketplacebe.entity.PaymentType.class))),
//            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
//            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
//            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
//            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
//            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
//    @PostMapping(
//            value = "/upload",
//            produces = {MediaType.APPLICATION_JSON_VALUE}
//    )
//    public ResponseEntity<List<it.pagopa.afm.marketplacebe.entity.PaymentType>> uploadPaymentTypeByList(
//            @RequestBody @Valid @NotNull List<String> paymentTypeList) {
//        return ResponseEntity.status(HttpStatus.CREATED).body(paymentTypeService.uploadPaymentTypeByList(paymentTypeList));
//    }

    @Operation(summary = "Sync payment types", security = {@SecurityRequirement(name = "ApiKey")}, tags = {"Payment Type",})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "OK", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE )),
            @ApiResponse(responseCode = "400", description = "Bad Request", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "Not Found", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping()
    public ResponseEntity<Void> syncPaymentTypes(
            @RequestBody List<it.pagopa.afm.marketplacebe.entity.PaymentType> paymentTypeList) {
        paymentTypeService.syncPaymentTypes(paymentTypeList);
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
